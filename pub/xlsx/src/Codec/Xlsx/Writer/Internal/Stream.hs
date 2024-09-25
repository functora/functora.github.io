{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Internal stream related functions.
--   These are exported because they're tested like this.
--   It's not expected a user would need this.
module Codec.Xlsx.Writer.Internal.Stream
  ( upsertSharedString,
    initialSharedString,
    SharedStringState (..),
  )
where

#ifdef USE_MICROLENS
import Lens.Micro.Platform
#else
import Control.Lens
#endif
import Control.Monad.State.Strict
import Data.Generics.Labels
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import GHC.Generics (Generic)

newtype SharedStringState = MkSharedStringState
  { string_map :: Map Text Int
  }
  deriving (Generic)

initialSharedString :: SharedStringState
initialSharedString = MkSharedStringState mempty

-- properties:
-- for a list of [text], every unique text gets a unique number.
upsertSharedString :: (MonadState SharedStringState m) => Text -> m (Text, Int)
upsertSharedString current = do
  strings <- use #string_map

  let mIdx :: Maybe Int
      mIdx = strings ^? ix current

      idx :: Int
      idx = fromMaybe (length strings) mIdx

      newMap :: Map Text Int
      newMap = at current ?~ idx $ strings

  #string_map .= newMap
  pure (current, idx)
