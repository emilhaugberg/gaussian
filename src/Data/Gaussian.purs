module Data.Gaussian where

import Data.Array (index, length, nubBy, zipWith, snoc, cons)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Prelude (class Eq, class Show, bind, flip, id, join, map, not, pure, show, negate, ($), (*), (+), (-), (/), (<$>), (<<<), (<>), (==), (=<<), (<))

type Row       = Array Number
newtype Matrix = Matrix (Array Row)

derive instance newtypeMatrix :: Newtype Matrix _

instance showMatrix :: Show Matrix where
  show m = "Matrix " <> show (unwrap m)

instance eqMatrix :: Eq Matrix where
  eq m1 m2 = unwrap m1 == unwrap m2

type Length = Int

indexFl :: forall a. Int -> Array a -> Maybe a
indexFl = flip index

subtractRow :: Row -> Row -> Row
subtractRow = zipWith (-)

findElement2 :: forall a. Int -> Int -> Array (Array a) -> Maybe a
findElement2 i j xs = join $ indexFl j <$> (indexFl i xs)

eliminate :: Length -> Int -> Int -> Matrix -> Maybe Matrix
eliminate length k i m = eliminate' m
  where
    eliminate' m = if k == length
      then pure $ id m
      else do
        kElem <- findElement2 k i m'
        iElem <- findElement2 i i m'
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
        jElem <- findElement2 j i m'

        if jElem == 0.0
          then pivot length (j + 1) i m
          else do
            iRow <- indexFl i m'
            jRow <- indexFl j m'

            pure $ Matrix (flipRows iRow jRow m')
    flipRows row1 row2 =
      map (\r -> if r == row1 then row2 else if r == row2 then row1 else id r)
    m' = unwrap m

gauss' :: Int -> Matrix -> Maybe Matrix
gauss' i m =
  if i == n - 1
    then pure $ id m
    else do
      iElem <- findElement2 i i m'

      if iElem == 0.0
        then do
          mP   <- pivot     n (i + 1) i m
          newM <- eliminate n (i + 1) i mP
          gauss' (i + 1) newM
        else do
          newM <- eliminate n (i + 1) i m
          gauss' (i + 1) newM
  where
    m'  = unwrap m
    n   = length m'

foreign import solver :: Int -> Matrix -> Array Number

gauss :: Matrix -> Either String (Array Number)
gauss m = if not $ isMatrix m
  then Left "Matrix incorrect"
  else maybe (Left "Couldn't calculate") Right result
  where
    result = solver length' <$> gauss' 0 m
    length' = (length $ unwrap m)
    isMatrix = (==) 1
           <<< length
           <<< nubBy (\xs ys -> length xs == length ys)
           <<< unwrap
