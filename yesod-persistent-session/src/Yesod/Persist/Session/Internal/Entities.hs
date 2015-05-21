module Yesod.Persist.Session.Internal.Entities
  ( PersistentSession(..)
  , PersistentSessionId
  , EntityField(..)
  , persistentSessionDefs
  ) where

import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Database.Persist (PersistEntity(..))
import Database.Persist.TH (mkPersist, mkSave, persistLowerCase, share, sqlSettings)

import Yesod.Persist.Session.Internal.Types (ByteStringJ, SessionId, SessionMapJ)


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
