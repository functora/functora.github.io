{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.Web
  ( PrvKey (..),
    ApiKey (..),
    RequestMethod (..),
    BaseUrl (..),
    withNonce,
    parseJsonBs,
    utcTimeToMicros,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS
import Functora.Cfg
import Functora.Prelude
import qualified Prelude

newtype PrvKey = PrvKey
  { unPrvKey :: Text
  }
  deriving newtype
    ( Eq,
      Ord,
      Read,
      IsString,
      HasCodec,
      HasItemCodec
    )
  deriving stock (Data, Generic)
  deriving (Show) via Redacted PrvKey

newtype ApiKey = ApiKey
  { unApiKey :: Text
  }
  deriving newtype
    ( Eq,
      Ord,
      Read,
      IsString,
      HasCodec,
      HasItemCodec
    )
  deriving stock (Data, Generic)
  deriving (Show) via Redacted ApiKey

data RequestMethod
  = GET
  | POST
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

newtype BaseUrl = BaseUrl
  { unBaseUrl :: Text
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Read,
      IsString
    )

parseJsonBs ::
  forall a.
  ( Typeable a,
    From (UTF_8 BS.ByteString) a
  ) =>
  A.Value ->
  A.Parser a
parseJsonBs =
  A.withText (inspectType @a)
    $ pure
    . via @(UTF_8 BS.ByteString)
