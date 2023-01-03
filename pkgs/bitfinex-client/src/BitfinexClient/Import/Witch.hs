{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Import.Witch
  ( module X,
    from,
    into,
    via,
    tryFrom,
    tryVia,
    composeTry,
    composeTryRhs,
    composeTryLhs,
  )
where

import Data.Type.Equality (type (==))
import Universum
import Witch as X
  ( From,
    TryFrom,
    TryFromException (..),
    withSource,
    withTarget,
  )
import qualified Witch

from ::
  forall source target.
  ( From source target,
    'False ~ (source == target)
  ) =>
  source ->
  target
from =
  Witch.from @source @target

into ::
  forall target source.
  ( From source target,
    'False ~ (source == target)
  ) =>
  source ->
  target
into =
  Witch.into @target @source

via ::
  forall through source target.
  ( From source through,
    From through target,
    'False ~ (source == through),
    'False ~ (through == target)
  ) =>
  source ->
  target
via =
  Witch.via @through @source @target

tryFrom ::
  forall source target.
  ( TryFrom source target,
    'False ~ (source == target)
  ) =>
  source ->
  Either (TryFromException source target) target
tryFrom =
  Witch.tryFrom @source @target

tryVia ::
  forall through source target.
  ( TryFrom source through,
    TryFrom through target,
    'False ~ (source == through),
    'False ~ (through == target)
  ) =>
  source ->
  Either (TryFromException source target) target
tryVia =
  tryFrom @through
    `composeTry` tryFrom

composeTry ::
  forall through source target.
  ( 'False ~ (source == through),
    'False ~ (through == target)
  ) =>
  ( through ->
    Either (TryFromException through target) target
  ) ->
  ( source ->
    Either (TryFromException source through) through
  ) ->
  source ->
  Either (TryFromException source target) target
composeTry =
  Witch.composeTry @through @source @target

composeTryRhs ::
  forall through source target.
  ( 'False ~ (source == through),
    'False ~ (through == target)
  ) =>
  (through -> target) ->
  ( source ->
    Either
      ( TryFromException source through
      )
      through
  ) ->
  source ->
  Either
    ( TryFromException source target
    )
    target
composeTryRhs =
  Witch.composeTryRhs @through @source @target

composeTryLhs ::
  forall through source target.
  ( 'False ~ (source == through),
    'False ~ (through == target)
  ) =>
  ( through ->
    Either
      ( TryFromException through target
      )
      target
  ) ->
  (source -> through) ->
  source ->
  Either
    ( TryFromException source target
    )
    target
composeTryLhs =
  Witch.composeTryLhs @through @source @target
