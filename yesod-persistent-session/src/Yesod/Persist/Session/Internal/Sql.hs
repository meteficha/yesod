module Yesod.Persist.Session.Internal.Sql
  ( PersistentSession(..)
  , PersistentSessionId
  , EntityField(..)
  , persistentSessionDefs
  , psKey
  , toPersistentSession
  , fromPersistentSession
  , SqlStorage(..)
  ) where

import Control.Monad (void)
import Data.Pool (Pool)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Database.Persist (PersistEntity(..), toPersistValue)
import Database.Persist.TH (mkPersist, mkSave, persistLowerCase, share, sqlSettings)

import qualified Database.Persist as P
import qualified Database.Persist.Sql as P

import Yesod.Persist.Session.Internal.Types


share
  [mkPersist sqlSettings, mkSave "persistentSessionDefs"]
  [persistLowerCase|
    PersistentSession json
      key       SessionId         -- Session ID, primary key.
      authId    ByteStringJ Maybe -- Value of "_ID" session key.
      session   SessionMapJ       -- Rest of the session data.
      createdAt UTCTime           -- When this session was created.
      Primary key
      deriving Eq Ord Show Typeable
  |]


-- | Generate a key to the entity from the session ID.
psKey :: SessionId -> Key PersistentSession
psKey = unwrap . keyFromValues . return . toPersistValue
  where
    unwrap (Left e) = error $
      "Yesod.Persist.Session.Internal.Entities.psKey: " ++
      "unexpected error from keyFromValues: " ++ show e
    unwrap (Right k) = k


-- | Convert from 'Session' to 'PersistentSession'.
toPersistentSession :: Session -> PersistentSession
toPersistentSession Session {..} =
  PersistentSession
    { persistentSessionKey       = sessionKey
    , persistentSessionAuthId    = fmap B sessionAuthId
    , persistentSessionSession   = M sessionData
    , persistentSessionCreatedAt = sessionCreatedAt
    }


-- | Convert from 'PersistentSession' to 'Session'.
fromPersistentSession :: PersistentSession -> Session
fromPersistentSession PersistentSession {..} =
  Session
    { sessionKey       = persistentSessionKey
    , sessionAuthId    = fmap unB persistentSessionAuthId
    , sessionData      = unM persistentSessionSession
    , sessionCreatedAt = persistentSessionCreatedAt
    }


-- | SQL session storage backend using @persistent@.
newtype SqlStorage =
  SqlStorage
    { connPool :: Pool P.SqlBackend
      -- ^ Pool of DB connections.  You may use the same pool as
      -- your application.
    } deriving (Typeable)


instance Storage SqlStorage where
  type TransactionM SqlStorage = P.SqlPersistT IO
  runTransactionM  = flip P.runSqlPool . connPool
  getSession     _ = fmap (fmap fromPersistentSession) . P.get . psKey
  deleteSession  _ = P.delete . psKey
  deleteAllSessionsOfAuthId _ authId = P.deleteWhere [PersistentSessionAuthId P.==. Just (B authId)]
  insertSession  _ = void . P.insert . toPersistentSession
  replaceSession _ = \session -> P.replace (psKey $ sessionKey session) $ toPersistentSession session
