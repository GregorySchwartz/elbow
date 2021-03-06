{- Math.Elbow
Gregory W. Schwartz

Collects the functions pertaining to the blah.
-}

{-# LANGUAGE TupleSections #-}

module Math.Elbow
  ( findElbow
  , findElbowList
  , getRotationAngle
  , MinMax (..)
  ) where

-- Remote
import Safe (headMay, atMay)
import qualified Numeric.LinearAlgebra as H

-- Local


data MinMax = Min | Max deriving (Read, Show)

-- | Convert a list to a tuple.
listToTuple :: [a] -> Maybe (a, a)
listToTuple [x, y] = Just (x, y)
listToTuple _      = Nothing

-- | Use the minimum and maximum points to identify the appropriate rotation in
-- radians. Matrix rows are observations, columns are dimensions.
getRotationAngle :: (RealFloat a, H.Numeric a) => H.Matrix a -> Maybe a
getRotationAngle = fmap (uncurry atan2)
                 . listToTuple
                 . reverse
                 . fmap (\x -> H.maxElement x - H.minElement x)
                 . H.toColumns

-- | Get the elbow point (and its index) of a two-dimensional distribution. Uses
-- the Max or Min to identify convex / concave point, depending on the choice
-- (for positive x and y, top left hump would be Max, bottom right dip would be
-- Min). Matrix rows are observations, columns are dimensions.
findElbow :: (RealFloat a, H.Numeric a)
          => MinMax -> H.Matrix a -> Maybe (Int, (a, a))
findElbow minMax m = do
  theta <- getRotationAngle m

  let co = cos theta
      si = sin theta
      rot = (2 H.>< 2) [co, -si, si, co]
      rotated = m H.<> rot
      findMinMax Max = H.maxIndex
      findMinMax Min = H.minIndex

  idx <- fmap (findMinMax minMax) . flip atMay 1 . H.toColumns $ rotated

  fmap (idx,) . listToTuple . H.toList $ m H.! idx

-- | Get the elbow point (and its index) of a two-dimensional distribution. Uses
-- the Max or Min to identify convex / concave point, depending on the choice
-- (for positive x and y, top left hump would be Max, bottom right dip would be
-- Min). Matrix rows are observations, columns are dimensions. List of columns.
findElbowList :: (RealFloat a, H.Numeric a)
              => MinMax -> [[a]] -> Maybe (Int, (a, a))
findElbowList minMax = findElbow minMax . H.fromColumns . fmap H.fromList
