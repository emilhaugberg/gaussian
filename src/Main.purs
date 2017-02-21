module Main where

import Data.Array
import Data.Newtype
import Data.Either
import Data.Gaussian (Matrix(..), gauss)
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Exception

showMatrix :: Matrix -> String
showMatrix (Matrix xs) = "Matrix: " <> show xs

main :: forall e. (Partial) => Eff (console :: CONSOLE, err :: EXCEPTION, ref :: REF | e) Unit
main = do
  log $ show $ fromRight $ gauss m
  log $ show $ length $ unwrap m
  where
    m = Matrix
      [ [ 2.0, 3.0,  13.0 ]
      , [ 1.0, -1.0, -1.0 ]
      ]
