module Lib where

import SemiPresiceConfigurator 
import Codec.Picture
import Data.List 
import System.IO
import Data.Matrix

demo :: [IO()]
demo = let
  base = "./samples/srcs/"
  names = [10..18]
  end = ".jpg" 
  end1 = ".png" in 
    map (\x -> singlecoef (base++(show x)++end) (base++(show x)++end1)) names

singlecoef :: FilePath -> FilePath -> IO()
singlecoef p p1= do
  Right image <- readImage p
  let pi = processImage image
      maxlen = maximum $ map length pi
      mtx = fromLists $ map (\x -> x++(replicate (maxlen - (length x)) 0)) pi
      pixelRenderer x y = PixelRGB8 (fromIntegral $ getElem (y+1) (x+1) mtx) 0 0 in -- * WE CAN STORE EVEN MORE DATA!!
        writeBitmap p1 $ generateImage pixelRenderer (ncols mtx) (nrows mtx)

compare :: FilePath -> FilePath -> FilePath -> IO()
compare p p1 p2 = do
  Right image1 <- readImage p
  Right image2 <- readImage p1
  let pi1 = (processImage image1) 
      pi2 = (processImage image2)
      pi1diffpi2 = zipWith (\\) pi1 pi2
      maxlen = maximum $ map length pi1diffpi2
      mtx = fromLists $ map (\x -> x++(replicate (maxlen - (length x)) 0)) pi1diffpi2
      pixelRenderer x y = PixelRGB8 (fromIntegral $ getElem (y+1) (x+1) mtx) 0 0 in -- * WE CAN STORE EVEN MORE DATA!!
        writeBitmap p2 $ generateImage pixelRenderer (ncols mtx) (nrows mtx)

