module Functora.Tags
  ( tag,
    unTag,
    reTag,
    module X,
  )
where

import Data.Kind as X (Type)
import Data.Tagged as X (Tagged (..))
import qualified Data.Tagged as Tagged
import Functora.TagsFamily as X
import Functora.TagsOrphan as X ()

tag ::
  forall tag tags rep.
  Tagged tags rep ->
  Tagged (tags |+| tag) rep
tag = Tagged.retag

unTag ::
  forall tag tags rep.
  Tagged tags rep ->
  Tagged (tags |-| tag) rep
unTag = Tagged.retag

reTag ::
  forall prev next tags rep.
  Tagged tags rep ->
  Tagged (tags |-| prev |+| next) rep
reTag = Tagged.retag
