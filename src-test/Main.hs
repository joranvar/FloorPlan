import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import Control.Arrow (first)
import Lib ()

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
  [ testGroup "SmallCheck" scTests
  , testGroup "Unit tests" huTests
  ]

scTests :: [TestTree]
scTests =
  [ testProperty "id x == x" $ \x -> uncurry (==) . first (id::Int->Int) $ (x, x)
  ]

huTests :: [TestTree]
huTests =
  [ testCase "True" $ uncurry (@?=) $ (True, True)
  ]
