{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CondFmtTests
  ( tests,
  )
where

import Codec.Xlsx
import Codec.Xlsx.Parser.Internal
import Codec.Xlsx.Writer.Internal
import Common
import Test.SmallCheck.Series.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Types.ConditionalFormatting tests"
    [ testProperty "fromCursor . toElement == id" $ \(cFmt :: CfRule) ->
        [cFmt] == fromCursor (cursorFromElement $ toElement (n_ "cfRule") cFmt)
    ]
