module Main where

import Data.Gaussian (Matrix(..), gauss)
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Exception

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION, ref :: REF | e) Unit
main = do
  x <- gauss m
  log $ show $ x
  where
    m = Matrix
      [ [ 1.0, 2.0, 3.0 ]
      , [ 2.0, 3.0, 4.0 ]
      , [ 2.0, 5.0, 6.0 ]
      ]
