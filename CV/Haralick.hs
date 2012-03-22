{-# LANGUAGE TypeOperators #-}
module CV.Haralick where

import CV.Image

import qualified Data.Map as M
import Data.Array.Repa as R
import Data.Array.Repa.Repr.Unboxed as R
import Data.List as L

type CoOccurrenceMatrix = R.Array U DIM2 Int

rToC :: Int -> D8 -> Int
rToC nColors r = round $ realToFrac r * (fromIntegral nColors - 1)

-- | Probably painfully slow implementation of gray level co-occurrence matrix
-- (GLCM) calculation to Repa DIM2 array.
--
coOccurrenceMatrix :: (Int,Int) -> Int -> Image GrayScale D8 -> CoOccurrenceMatrix
coOccurrenceMatrix (dx,dy) nColors i =
    let (w,h)  = getSize i
        usableWidth  = if dx > 0 then (w-dx) else w
        usableHeight = if dy > 0 then (h-dy) else h
        startX       = if dx < 0 then (-dx) else 0
        startY       = if dy < 0 then (-dy) else 0
        getColor     = rToC nColors . (flip getPixel) i
        -- Force indices (row,col) to hit bottom triangle of the symmetric DIM2 matrix.
        swap' (r,c) = if r<c then (c,r) else (r,c)
        fuu (x,y)    = swap' $ (getColor (x,y), getColor(x+dx,y+dy))
        neighbours   = [ fuu (x,y)
                            | x <- [startX..(usableWidth-1)]
                            , y <- [startY..(usableHeight-1)] ]
        sums :: M.Map (Int,Int) Int
        -- XXX: Neighbours sorted as list and packed to temporary Map.
        sums = M.fromList . Prelude.map (\x -> (head x, length x)) . group . sort $ neighbours
        shape = Z :. nColors :. nColors
    in R.computeUnboxedP $ R.fromFunction shape $ \(Z:.r:.c) ->
                            case M.lookup (swap' (r,c)) sums of
                                Just sum' -> sum'
                                _         -> 0
