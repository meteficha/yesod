module Yesod.Persist.Session.Internal.Backend
  ( State(..)
  , createState
  , backend
  , loadSession
  , invalidateIfNeeded
  , DecomposedSession
  , decomposeSession
  , saveSessionOnDb
  , createCookie
  , findSessionId
  , toSessionMap
  , authKey
  , forceInvalidateKey
  , ForceInvalidate(..)
  , forceInvalidate
  ) where

import Control.Monad (guard, void, when)
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.Maybe (isJust)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Typeable (Typeable)
import Web.Cookie (parseCookies, SetCookie(..))
import Yesod.Core
import Yesod.Core.Types (SaveSession)

import qualified Crypto.Nonce as N
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Data.Text.Encoding as TE
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import qualified Network.Wai as W

import Yesod.Persist.Session.Internal.Entities
import Yesod.Persist.Session.Internal.Types

-- TODO: expiration

-- | The server-side session backend needs to maintain some state
-- in order to work:
--
--   * A nonce generator for the session IDs.
--
--   * A SQL connection pool for saving and loading the sessions.
--
-- Create a new 'State' using 'createState'.
data State =
  State
    { generator :: !N.Generator
    , connPool  :: !(Pool P.SqlBackend)
    } deriving (Typeable)


-- | Create a new 'State' for the server-side session backend
-- using the given pool of SQL connections.  You may use the same
-- pool as your application.
createState :: MonadIO m => Pool P.SqlBackend -> m State
createState pool = State <$> N.new <*> return pool


-- | Construct the server-side session backend from the given state.
backend :: State -> SessionBackend
backend state =
  SessionBackend {
    sbLoadSession = loadSession state "JSESSIONID" -- LOL :)
  }


-- | Load the session map from the DB from the ID on the request.
-- Also provides a function to update the session when sending
-- the response.
loadSession :: State -> ByteString -> W.Request -> IO (SessionMap, SaveSession)
loadSession state cookieName = load
  where
    runDB :: P.SqlPersistT IO a -> IO a
    runDB act = P.runSqlPool act (connPool state)

    load :: W.Request -> IO (SessionMap, SaveSession)
    load req = do
      -- Find 'SessionId' (if any) and load it from DB (if present).
      let maybeInputId = findSessionId cookieName req
      maybeInput <- maybe (return Nothing) (runDB . P.get . psKey) maybeInputId
      let inputSessionMap = maybe M.empty toSessionMap maybeInput
      return (inputSessionMap, save maybeInput)

    save :: Maybe PersistentSession -> SaveSession
    save maybeInput wholeOutputSessionMap =
      runDB $ do
        let decomposedSessionMap = decomposeSession wholeOutputSessionMap
        newMaybeInput <- invalidateIfNeeded maybeInput decomposedSessionMap
        key <- saveSessionOnDb state newMaybeInput decomposedSessionMap
        return [createCookie cookieName key]


-- | Invalidates an old session ID if needed.  Returns the
-- 'PersistentSession' that should be replaced when saving
-- the session, if any.
--
-- Currently we invalidate whenever the auth ID has changed
-- (login, logout, different user) in order to prevent session
-- fixation attacks.  We also invalidate when asked to via
-- 'forceInvalidate'.
invalidateIfNeeded :: Maybe PersistentSession -> DecomposedSession -> P.SqlPersistT IO (Maybe PersistentSession)
invalidateIfNeeded maybeInput DecomposedSession {..} = do
  -- Decide which action to take.
  -- "invalidateOthers implies invalidateCurrent" should be true below.
  let inputAuthId       = persistentSessionAuthId =<< maybeInput
      invalidateCurrent = dsForceInvalidate /= DoNotForceInvalidate || inputAuthId /= dsAuthId
      invalidateOthers  = dsForceInvalidate == AllSessionIdsOfLoggedUser && isJust dsAuthId
      whenMaybe b m f   = when b $ maybe (return ()) f m
  -- Delete current and others, as requested.
  whenMaybe invalidateCurrent maybeInput $ P.delete . psKey . persistentSessionKey
  whenMaybe invalidateOthers  dsAuthId   $ \a -> P.deleteWhere [PersistentSessionAuthId P.==. Just a]
  -- Remember the input only if not invalidated.
  return $ guard (not invalidateCurrent) >> maybeInput


-- | A 'SessionMap' with its 'authKey' taken apart.
data DecomposedSession =
  DecomposedSession
    { dsAuthId          :: !(Maybe ByteStringJ)
    , dsForceInvalidate :: !ForceInvalidate
    , dsSessionMap      :: !SessionMapJ
    } deriving (Show, Typeable)


-- | Decompose a session (see 'DecomposedSession').
decomposeSession :: SessionMap -> DecomposedSession
decomposeSession sm1 =
  let (authId, sm2) = M.updateLookupWithKey (\_ _ -> Nothing) authKey            sm1
      (force,  sm3) = M.updateLookupWithKey (\_ _ -> Nothing) forceInvalidateKey sm2
  in DecomposedSession
       { dsAuthId          = B <$> authId
       , dsForceInvalidate = maybe DoNotForceInvalidate (read . B8.unpack) force
       , dsSessionMap      = M sm3 }


-- | Save a session on the database.  If an old session is
-- supplied, it is replaced, otherwise a new session is
-- generated.
saveSessionOnDb
  :: State
  -> Maybe PersistentSession     -- ^ The old session, if any.
  -> DecomposedSession           -- ^ The session data to be saved.
  -> P.SqlPersistT IO SessionId  -- ^ The ID of the saved session.
saveSessionOnDb state maybeInput DecomposedSession {..} = do
  -- Generate properties if needed or take them from previous
  -- saved session.
  (saveToDb, key, createdAt) <-
    case maybeInput of
      Nothing -> liftIO $
        (,,) <$> return (void . P.insert)
             <*> generateSessionId (generator state)
             <*> getCurrentTime
      Just PersistentSession {..} ->
        return ( P.replace (psKey persistentSessionKey)
               , persistentSessionKey
               , persistentSessionCreatedAt)
  -- Save to the database.
  saveToDb $ PersistentSession key dsAuthId dsSessionMap createdAt
  return key


-- | Create a cookie for the given session ID.
createCookie :: ByteString -> SessionId -> Header
createCookie cookieName key =
  -- Generate a cookie with the final session ID.
  AddCookie def
    { setCookieName     = cookieName
    , setCookieValue    = TE.encodeUtf8 $ unS key
    , setCookiePath     = Just "/"
    , setCookieExpires  = Just undefined
    , setCookieDomain   = Nothing
    , setCookieHttpOnly = True
    }


-- | Fetch the 'SessionId' from the cookie with the given name.
-- Returns @Nothing@ if:
--
--   * There are zero cookies with the given name.
--
--   * There is more than one cookie with the given name.
--
--   * The cookie's value isn't considered a 'SessionId'.  We're
--   a bit strict here.
findSessionId :: ByteString -> W.Request -> Maybe SessionId
findSessionId cookieName req = do
  let matching = do
        ("Cookie", header) <- W.requestHeaders req
        (k, v) <- parseCookies header
        guard (k == cookieName)
        return v
  [raw] <- return matching
  fromPathPiece (TE.decodeUtf8 raw)


-- | Create a 'SessionMap' from a 'PersistentSession'.
toSessionMap :: PersistentSession -> SessionMap
toSessionMap PersistentSession {..} =
  maybe id (\(B v) -> M.insert authKey v) persistentSessionAuthId $ -- Insert auth key (if any).
  unM persistentSessionSession                                      -- Remove newtype layer.


-- | The session key used by @yesod-auth@ without depending on it.
authKey :: Text
authKey = "_ID"


-- | The session key used to signal that the session ID should be
-- invalidated.
forceInvalidateKey :: Text
forceInvalidateKey = "yesod-persistent-session-force-invalidate"


-- | Which session IDs should be invalidated.
data ForceInvalidate =
    CurrentSessionId
    -- ^ Invalidate the current session ID.  The current session
    -- ID is automatically invalidated on @yesod-auth@ login and
    -- logout.
  | AllSessionIdsOfLoggedUser
    -- ^ Invalidate all session IDs beloging to the currently
    -- logged in user.  Only the current session ID will be
    -- renewed (the only one for which a cookie can be set).
    --
    -- This is useful, for example, if the user asks to change
    -- their password.  It's also useful to provide a button to
    -- clear all other sessions.
    --
    -- If the user is not logged in, this option behaves exactly
    -- as 'CurrentSessionId' (i.e., it /does not/ invalidate the
    -- sessions of all logged out users).
    --
    -- Note that, for the purposes of
    -- 'AllSessionIdsOfLoggedUser', we consider \"logged user\"
    -- the one that is logged in at the *end* of the handler
    -- processing.  For example, if the user was logged in but
    -- the current handler logged him out, the session IDs of the
    -- user who was logged in will not be invalidated.
  | DoNotForceInvalidate
    -- ^ Do not force invalidate.  Invalidate only if
    -- automatically.  This is the default.
    deriving (Eq, Ord, Show, Read, Enum, Typeable)


-- | Invalidate the current session ID (and possibly more, check
-- 'ForceInvalidate').  This is useful to avoid session fixation
-- attacks (cf. <http://www.acrossecurity.com/papers/session_fixation.pdf>).
--
-- Note that the invalidate /does not/ occur when the call to
-- this action is made!  The sessions will be invalidated on the
-- end of the handler processing.  This means that later calls to
-- 'forceInvalidate' on the same handler will override earlier
-- calls.
forceInvalidate :: MonadHandler m => ForceInvalidate -> m ()
forceInvalidate = setSessionBS forceInvalidateKey . B8.pack . show
