{-# LANGUAGE ForeignFunctionInterface #-}
module CV.Bindings.Features where

import Foreign.C.Types
import CV.Bindings.Types

#strict_import

#include <bindings.dsl.h>
#include <opencv/cv.h>

#starttype CvSURFParams
#field extended, Int
#field hessianThreshold, Double
#field nOctaves, Int
#field nOctaveLayers, Int
#stoptype

#ccall wrapExtractSURF, Ptr <CvArr> -> Ptr <CvArr> -> Ptr (Ptr <CvSeq>) -> Ptr (Ptr <CvSeq>) -> Ptr <CvMemStorage> -> Ptr <CvSURFParams> -> Int -> IO ()