module Yesod.Persist.Session
  (
  ) where

import Data.Typeable (Typeable)

import qualified Crypto.Nonce as N

-- | TODO
data PersistentSessionState =
  PersistentSessionState {
    generator :: !N.Generator
  } deriving (Typeable)
