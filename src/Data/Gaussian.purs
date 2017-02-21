module Data.Gaussian where

import Data.Array
import Data.Either
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable
import Data.Tuple (Tuple(..), fst, snd)
import Prelude

type Row       = Array Number
newtype Matrix = Matrix (Array Row)

derive instance newtypeMatrix :: Newtype Matrix _

type Length = Int

indexFl :: forall a. Int -> Array a -> Maybe a
indexFl = flip index

subtractRow :: Row -> Row -> Row
subtractRow = zipWith (-)

eliminate :: Length -> Int -> Int -> Matrix -> Maybe Matrix
eliminate length k i m = eliminate' m
  where
    eliminate' m = if k == length
      then pure $ id m
      else do
        kElem <- join $ indexFl i <$> (indexFl k m')
        iElem <- join $ indexFl i <$> (indexFl i m')
        kRow  <- indexFl k m'
        iRow  <- indexFl i m'

        let r         = kElem / iElem
        let newKRow   = subtractRow (map ((*) r) iRow) kRow
        let newRows   = map (\r -> if r == kRow then newKRow else id r) m'

        eliminate length (k + 1) i (Matrix (newRows))
    m' = unwrap m

pivot :: Length -> Int -> Int -> Matrix -> Maybe Matrix
pivot length j i m = pivot' m
  where
    pivot' m = if j == length
      then pure $ id m
      else do
        jElem <- join $ indexFl i <$> (indexFl j m')

        if jElem == 0.0
          then pivot length (j + 1) i m
          else do
            iRow <- indexFl i m'
            jRow <- indexFl j m'

            pure $ Matrix (flip iRow jRow m')
    flip row1 row2 =
      map (\r -> if r == row1 then row2 else if r == row2 then row1 else id r)
    m' = unwrap m

gauss' :: Int -> Matrix -> Maybe Matrix
gauss' i m =
  if i == n - 1
    then pure $ id m
    else do
      iElem <- join $ indexFl i <$> (indexFl i m')

      if iElem == 0.0
        then do
          mP   <- pivot     n (i + 1) i m
          newM <- eliminate n (i + 1) i m
          gauss' (i + 1) newM
        else do
          newM <- eliminate n (i + 1) i m
          gauss' (i + 1) newM
  where
    m'  = unwrap m
    nI  = length m' - 1
    n   = length m'

addToM :: Array Number -> Matrix -> Matrix
addToM xs (Matrix m) = Matrix $ snoc m xs

foreign import solver :: Int -> Matrix -> Array Number

gauss :: Matrix -> Either String (Array Number)
gauss m = if not $ isMatrix m
  then Left "Matrix incorrect"
  else maybe (Left "wrong") Right (solver (length $ unwrap m) <$> gauss' 0 m)
  where
    isMatrix = (==) 1
           <<< length
           <<< nubBy (\xs ys -> length xs == length ys)
           <<< unwrap
