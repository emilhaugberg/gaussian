module Main where

import Data.Either (Either)
import Data.Gaussian (Matrix, gauss)

main :: Matrix -> Either String (Array Number)
main = gauss
