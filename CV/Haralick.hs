{-# LANGUAGE RecordWildCards #-}
module CV.Haralick where

import CV.Image
import CV.ColourUtils
import CV.Pixelwise
import qualified Data.Vector as V

data GLCM = GLCM { hist :: V.Vector Double
                 , nColors :: Int } deriving Show

--FIX Getpixel olettaa 256 väriä ja käyttää suoraan indeksinä

calcGLCM :: (Int,Int) -> Int -> Image GrayScale D8 -> GLCM
calcGLCM (dx,dy) nColors x =
    let get (i,j)  = fromIntegral $ getPixel (i,j) x
        (w,h) = getSize x

        hist = V.accum (+) (V.replicate (toIdx (nColors-1,nColors-1)) 0) $
                [(toIdx (get (i,j), get (i+dx,j+dy)),1)
                | i <- [0..w-1-dx]
                , j <- [0..h-1-dy]]
    in GLCM{..}

toIdx (i,j) = (max i j*(max i j+1)) `div` 2 + min i j

glcmToImage :: GLCM -> Image GrayScale D32
glcmToImage g@GLCM{..} = logarithmicCompression $ imageFromFunction (a,a) (realToFrac . (V.!) hist . toIdx)
  where
    a = nColors - 1
