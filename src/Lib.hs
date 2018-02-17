module Lib where

import SemiPresiceConfigurator 
import Codec.Picture
import Data.List 
import System.IO

demo :: IO()
demo = do
  Right image1 <- readImage ("./tost.jpg"::FilePath)
  Right image2 <- readImage ("./tostotito.jpg"::FilePath)
  Right image3 <- readImage ("./toost.jpg"::FilePath)
  Right image4 <- readImage ("./teest.jpg"::FilePath)
  putStrLn $ " default>     "   ++ (show $ (processImage image1))       ++
             "\n lenschanged> " ++ (show $ (processImage image2))       ++
             "\n dsparks>     " ++ (show $ (processImage image3))       ++
             "\n differentone>" ++ (show $ (processImage image4))       ++
             "\n diff12>                           " ++ (show $ (processImage image1) \\ (processImage image2)) ++
             "\n diff23>                           " ++ (show $ (processImage image2) \\ (processImage image4)) ++
             "\n diff34>                           " ++ (show $ (processImage image3) \\ (processImage image4)) 
-- !!! РАЗНОСТЬ (РАЗНИЦА, DIFF) СПИСКОВ ЕСТЬ КОЭФФИЦИЕНТ РАЗЛИЧИЯ