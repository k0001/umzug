module Umzug.Aeson
 ( aesonCodec
 ) where

import Control.Monad (join)
import qualified Data.Aeson as Ae
import qualified Pipes.Aeson.Unchecked

import qualified Umzug as U

--------------------------------------------------------------------------------

-- | Serializes @x@ using its 'Ae.ToJSON'/'Ae.FromJSON' representation.
aesonCodec
  :: (Monad em, Monad dm, Ae.ToJSON x, Ae.FromJSON x)
  => U.Codec em dm x
aesonCodec = U.Codec
  { U.encode = Pipes.Aeson.Unchecked.encode
  , U.decode = fmap (join . fmap (either (const Nothing) Just))
                    Pipes.Aeson.Unchecked.decode
  }
