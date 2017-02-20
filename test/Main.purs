module Test.Main where

import Control.Monad.Eff.Console (log)
import Test.Unit.Assert as Assert
import Data.Gaussian (Matrix(..), isSufficient)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)

main = runTest do
  suite "basic functions" do
    test "All rows in matrix have same length" do
      Assert.equal (isSufficient m) true
  where
    m = Matrix [ [1.0, 2.0, 3.0]
               , [2.0, 3.0, 4.0]
               , [4.0, 5.0, 1.0]
               ]
-- main = do
--   log "hey"
