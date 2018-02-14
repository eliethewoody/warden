{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}

module SemiPresiceConfigurator (processImage) where

import Codec.Picture
import Control.Monad.ST
import Data.Word(Word8)
import Data.Bits(xor)
import System.IO
import Data.List.Split
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Codec.Picture.Types as PT

muchGreater :: (Integral a, Ord a, Eq a, Num a) => a -> a -> Bool
muchGreater 0 b = False
muchGreater a 0 = True
muchGreater a b
  | a > b+(b `div` 2) = True -- ! kinda trouble here. This is a test value, has to be adjusted after the testing
  | a == b               = False
  | b < a                = False
  | otherwise            = False

peaksOf :: (Integral a, Ord a, Eq a, Num a) => [a] -> [a]
peaksOf l = let triplets = chunksOf 3 l in 
  filter (/=0) $ map (\(x:y:z:_) -> if (y `muchGreater` x) && (y `muchGreater` z) then y else 0) triplets

contextSensitiveCoeffOf :: [Word8] -> Int
contextSensitiveCoeffOf row = let 
  peaks = peaksOf row in 
    if (length peaks) > 0 then
      (length row)*
      ((sum . map fromIntegral) peaks)`div`
      ((length peaks)*
      ((sum . map fromIntegral) row))
    else (length row)`div`((sum . map fromIntegral) row)

foldDataTile :: BS.ByteString -> Int -> Int
foldDataTile s w =
 let image = chunksOf w (BS.unpack s)
     in sum $ map contextSensitiveCoeffOf image

imageChunkOf :: Image PixelRGB8 -> (Int, Int) -> (Int, Int) -> Image PixelRGB8 -- * in this functions all coordinates are counted from 0 and ImageWidth\Height are counted from the 1.
imageChunkOf source@Image{..} (x1, y1) (x2, y2) = if x1 <= imageWidth && 
                                                     x2 <= imageWidth &&
                                                     y1 <= imageHeight && 
                                                     y2 <= imageHeight &&
                                                     x2 - x1 <= imageWidth &&
                                                     y2 - y1 <= imageHeight &&
                                                     x2 - x1 > 0 &&
                                                     y2 - y1 > 0
  then 
    runST $ do
      mImg <- PT.newMutableImage (x2 - x1) (y2 - y1)
      let step x y 
            | x >= (x2) = step x1 (y+1)
            | y >= (y2) = PT.unsafeFreezeImage mImg
            | otherwise = do -- do inside do inside a runST inside an IF!!
              writePixel mImg (x-x1) (y-y1) (pixelAt source x y)
              step (x + 1) y
          in step x1 y1 
  else error "Oh NO!"

-- //processImage :: DynamicImage -> [Int]
processImage :: DynamicImage -> [Int]
processImage img = let
      image = convertRGB8 img 
      -- * assume that the image would be analysed as a grid 8x8 cells\data_tiles. 
      -- * so the w and h parameters should be divided by 8 to get the dimentions 
      -- * of the data_tile being analysed.
      w       = (imageWidth image)
      h       = (imageHeight image)
      wt      = w `div` 8  
      ht      = h `div` 8 
      widths  = [wt*x|x<-[0..7]]
      heights = [ht*x|x<-[0..7]]
      -- //diagTiles   = zipWith (\x y -> imageChunkOf image (x, y) ((x+wt), (y+ht))) widths heights in 
      tiles = [imageChunkOf image (x, y) ((x+wt), (y+ht))|x<-widths,y<-heights] 
      -- //zipWith (\i n -> writeBitmap ("./imgs/"++(show n)++".bmp"::FilePath) i) tiles [0..(length tiles)] 
      bitmaps = map encodeBitmap tiles in
      -- //foldl (addDigit) 0 $ map (\x -> foldDataTile (BSL.toStrict x) wt) bitmaps
      map (\x -> foldDataTile (BSL.toStrict x) wt) bitmaps
      
      
-- //coeffOf :: FilePath -> IO()
-- //coeffOf fp = do
-- //  eimg <- readImage fp
-- //  case eimg of
-- //    Left err -> error err
-- //    Right img ->
-- //      let a = processImage img in print a
-- // convertRGB8 :: DynamicImage -> Image PixelRGB8