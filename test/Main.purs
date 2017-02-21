module Test.Main where

import Control.Monad.Eff.Console (log)
import Test.Unit.Assert as Assert
import Data.Gaussian (Matrix(..), gauss)
import Data.Either (fromRight)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Partial.Unsafe (unsafePartial)
import Prelude (negate, bind, ($))

main = unsafePartial $ runTest do
  suite "algorithm" do
    test "Outcome" do
      Assert.equal (fromRight $ gauss m1) [ 2.0, 3.0 ]
      Assert.equal (fromRight $ gauss m2) [ -1.0, 4.0, -1.0 ]
  where
    m1 = Matrix [ [ 2.0, 3.0,  13.0 ]
                , [ 1.0, -1.0, -1.0 ]
                ]

    m2 = Matrix [ [  1.0, 0.0, -1.0, 0.0 ]
                , [  3.0, 1.0,  0.0, 1.0 ]
                , [ -1.0, 1.0,  1.0, 4.0 ]
                ]
