import Criterion.Main
import Functora.Prelude
import Test.QuickCheck hiding (Fixed)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (newQCGen)

mkSamples :: Int -> IO [Fix]
mkSamples n = do
  gen <- newQCGen
  pure . fmap Fix $ unGen (vectorOf n arbitrary) gen n

main :: IO ()
main = do
  samples <- mkSamples 1000000
  defaultMain
    [ bgroup
        "Fix -> Double"
        [ bench "via Rational" $
            whnf (map $ from @Fix @Double) samples
        ]
    ]
