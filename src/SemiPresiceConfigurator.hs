{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE FlexibleContexts #-}

module SemiPresiceConfigurator where

import Codec.Picture
import Control.Monad.ST
import Data.Word(Word8)
import Data.Matrix
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
  | a > b+(b `div` bias) = True 
  | a == b               = False
  | b < a                = False
  | otherwise            = False where 
    bias                 = 10 -- ! kinda trouble here. This is a test value, has to be adjusted after the testing

  -- ! {Li when L(i-1)<8\9*Li>L(i-1)}
   
peaksOf :: (Integral a, Ord a, Eq a, Num a) => [a] -> [a]
peaksOf l = let triplets = chunksOf l in 
  map (\(x:y:z:_) -> if (y `muchGreater` x) && (y `muchGreater` z) then y else 0) triplets


contextSensitiveCoeffOf :: [Word8] -> Int
contextSensitiveCoeffOf row =
 let avg a = sum a `div` length a -- the average value of the list
     apv = (avg . findPeaks . map fromIntegral) row
 in apv `div` ((avg . map fromIntegral) row)

foldDataTile :: BS.ByteString -> Int -> Int
foldDataTile s w =
 let image = toLists $ fromList (BS.length s `div` w) w (BS.unpack s)
     in sum $ map contextS*ensitiveCoeffOf image

chunkImage :: Int -> Int -> Int -> Int -> Image PixelRGB8 -> Image PixelRGB8
chunkImage w h sx sy image@Image{..} =
  if w      < imageWidth  &&
     h      < imageHeight &&
     sx     < imageWidth  &&
     sy     < imageHeight &&
     (sx+w) < imageWidth  &&
     (sy+h) < imageHeight
  then runST $ do
    mutableImage <- PT.newMutableImage w h
    let go x y
          | x == (w+sx) = go sx (y+1)
          | y == (h+sy) = PT.unsafeFreezeImage mutableImage
          | otherwise = do
            writePixel mutableImage (x-sx) (y-sy) (pixelAt image x y)
            go (x+1) y
       in go sx sy
  else error "Wrong img params passed"

processImage :: FilePath -> IO [Int] -- !FIXME: rewrite, looks ugly 
processImage fp = do
  eimg <- readImage fp
  case eimg of
    Left err -> error err
    Right img -> do 
      image <- convertRGB8 img 
      -- * assume that the image would be analysed as a grid 8x8 cells\data_tiles. 
      -- * so the w and h parameters should be divided by 8 to get the dimentions 
      -- * of the data_tile being analysed.
      wt      <- (imageWidth image)  `div` 8 -- ! static coeff here 
      ht      <- (imageHeight image) `div` 8 -- ! static coeff here
      widths  <- [x | x <- n*wt,
                      n <- [0..((imageWidth image) `div` wt)]]
      heights <- [x | x <- n*ht,
                      n <- [0..((imageHeight) `div` ht)]]
      tiles   <- zipWith (\x y -> chunkImage x y (x+wt) (y+ht) image) widths heights
      bitmaps <- map encodeBitmap tiles
      map (\x -> foldDataTile (BSL.toStrict x) wt) bitmaps
      

--convertRGB8 :: DynamicImage -> Image PixelRGB8