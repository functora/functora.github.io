{-# LANGUAGE DeriveGeneric #-}

module Codec.Xlsx.Types.Comment where

import Codec.Xlsx.Types.Common
import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | User comment for a cell
--
-- TODO: the following child elements:
-- guid, shapeId, commentPr
--
-- Section 18.7.3 "comment (Comment)" (p. 1749)
data Comment = Comment
  { -- | cell comment text, maybe formatted
    -- Section 18.7.7 "text (Comment Text)" (p. 1754)
    _commentText :: XlsxText,
    -- | comment author
    _commentAuthor :: Text,
    _commentVisible :: Bool
  }
  deriving (Eq, Show, Generic)

instance NFData Comment
