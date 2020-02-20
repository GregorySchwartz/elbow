{- Math.Elbow
Gregory W. Schwartz

Collects the functions pertaining to the blah.
-}

{-# LANGUAGE TupleSections #-}

module Math.Elbow
  ( findElbow
  ) where

-- Remote
import Safe (headMay, atMay)
import qualified Numeric.LinearAlgebra as H

-- Local

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

-- | Get the elbow point (and its index) of a two-dimensional distribution.
-- Matrix rows are observations, columns are dimensions.
findElbow :: (RealFloat a, H.Numeric a) => H.Matrix a -> Maybe (Int, (a, a))
findElbow m = do
  theta <- getRotationAngle m

  let co = cos theta
      si = sin theta
      rot = (2 H.>< 2) [co, -si, si, co]
      rotated = m H.<> rot

  idx <- fmap H.minIndex . flip atMay 1 . H.toColumns $ rotated

  fmap (idx,) . listToTuple . H.toList $ m H.! idx
