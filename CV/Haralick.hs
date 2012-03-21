module CV.Haralick where

import CV.Image

import qualified Data.Array.CArray as CA

type CoOccurenceMatrix = CA.CArray (Int,Int) Double

ncolors = 256

rToC :: Double -> Int
rToC r = round $ r * (nColors-1)

-- | Use stretchHistogram if needed before using this.
coOccurrenceMatrix :: (Int,Int) -> Image GrayScale D8 -> CoOccurrenceMatrix
coOccurrenceMatrix (dx,dy) i =
    let (w,h)  = getSize i
        usableWidth  = if dx > 0 then (w-dx) else w
        usableHeight = if dy > 0 then (h-dy) else h
        startX       = if dx < 0 then (-dx) else 0
        startY       = if dy < 0 then (-dy) else 0
        getColor = rToC . getPixel
        relations = [ (getColor (x,y), getColor(x+dx,y+dy))
                            | x <- [startX..(usableWidth-1)]
                            , y <- [startY..(usableHeight-1)] ]

