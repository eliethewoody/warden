module Lib where

import SemiPresiceConfigurator 
import Codec.Picture
import Data.List 

demo :: IO()
demo = do 
  Right image1 <- readImage "./tost.jpg"
  Right image2 <- readImage "./tostotito.jpg"
  Right image3 <- readImage "./toost.jpg"
  Right image4 <- readImage "./teest.jpg"
  i1           <- processImage image1
  i2           <- processImage image2
  i3           <- processImage image3
  i4           <- processImage image3
  print    $ "i1default> "       ++ (show $ i1)       ++
             "\ni2lenschanged> " ++ (show $ i2)       ++
             "\ni3dsparks> "     ++ (show $ i3)       ++
             "\ni4differentone> "++ (show $ i4)       ++
             "\ndiff12>"         ++ (show $ i1 \\ i2) ++
             "\ndiff23>"         ++ (show $ i2 \\ i3) ++
             "\ndiff34>"         ++ (show $ i3 \\ i4) 
-- !!! РАЗНОСТЬ (РАЗНИЦА, DIFF) СПИСКОВ ЕСТЬ КОЭФФИЦИЕНТ РАЗЛИЧИЯ