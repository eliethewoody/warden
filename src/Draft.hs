{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Codec.Picture
import System.Environment(getArgs)
import Control.Monad.ST
import Data.Word(Word8)
import Data.Matrix
import Data.Bits(xor)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Codec.Picture.Types as PT


someFunc :: IO ()
someFunc = do
  eimga <- readImage "a.png"
  eimgb <- readImage "b.png"
  eimgc <- readImage "c.png"
  eimgd <- readImage "d.png"
  eimge <- readImage "e.png"
  eimgf <- readImage "f.png"
  case eimga of
    Left err    -> putStrLn ("Could not read image: " ++ err)
    --Right (img) -> (savePngImage "b.png" . ImageRGB8 . chunkImage 50 50 50 50) (convertRGB8 img)
    --Right img -> saveImageAsQuaters img 149 149
    Right img -> f img 149 "a" 
  case eimgb of
    Left err    -> putStrLn ("Could not read image: " ++ err)
    --Right (img) -> (savePngImage "b.png" . ImageRGB8 . chunkImage 50 50 50 50) (convertRGB8 img)
    --Right img -> saveImageAsQuaters img 149 149
    Right img -> f img 149 "b" 
  case eimgc of
    Left err    -> putStrLn ("Could not read image: " ++ err)
    --Right (img) -> (savePngImage "b.png" . ImageRGB8 . chunkImage 50 50 50 50) (convertRGB8 img)
    --Right img -> saveImageAsQuaters img 149 149
    Right img -> f img 149 "c" 
  case eimgd of
    Left err    -> putStrLn ("Could not read image: " ++ err)
    --Right (img) -> (savePngImage "b.png" . ImageRGB8 . chunkImage 50 50 50 50) (convertRGB8 img)
    --Right img -> saveImageAsQuaters img 149 149
    Right img -> f img 149 "d" 
  case eimge of
    Left err    -> putStrLn ("Could not read image: " ++ err)
    --Right (img) -> (savePngImage "b.png" . ImageRGB8 . chunkImage 50 50 50 50) (convertRGB8 img)
    --Right img -> saveImageAsQuaters img 149 149
    Right img -> f img 149 "e" 
  case eimgf of
    Left err    -> putStrLn ("Could not read image: " ++ err)
    --Right (img) -> (savePngImage "b.png" . ImageRGB8 . chunkImage 50 50 50 50) (convertRGB8 img)
    --Right img -> saveImageAsQuaters img 149 149
    Right img -> f img 450 "f" 

saveImageAsQuaters :: DynamicImage -> Int -> Int -> IO ()
saveImageAsQuaters s mx my = do
    (savePngImage "lu.png" . ImageRGB8 . chunkImage mx my 0  0 ) (convertRGB8 s)
    (savePngImage "ru.png" . ImageRGB8 . chunkImage mx my mx 0 ) (convertRGB8 s)
    (savePngImage "ld.png" . ImageRGB8 . chunkImage mx my 0  my) (convertRGB8 s)
    (savePngImage "rd.png" . ImageRGB8 . chunkImage mx my mx my) (convertRGB8 s) 
    
f :: DynamicImage -> Int -> String -> IO() 
f s w l = 
  let btm = encodeDynamicBitmap s
  in case btm of 
    Left err -> putStrLn ("arbetirnet : arrattaa " ++ err)
    Right b -> do
      print l  
      print $ foldDataTile (BSL.toStrict b) w

muchGreater :: (Integral a, Ord a, Eq a, Num a) => a -> a -> Bool
muchGreater 0 b = False
muchGreater a 0 = True
muchGreater a b
  | a > b+(b `div` 8) = True
  | a == b            = False
  | b < a             = False
  | otherwise         = False

findPeaks :: (Integral a, Ord a, Eq a, Num a) => [a] -> [a] --FIXME: kinda weird algoritm
findPeaks src =
  let step [] p = (reverse p)
      step (a:l) p =
        if ((l /= []) && (a `muchGreater` (head l)))
          then step l (a:p)
          else step l p
      peaks = step src []
      in if (length peaks) == 0
        else peaks
         then [0]

contextSensitiveCoeffOf :: [Word8] -> Int
contextSensitiveCoeffOf row =
 let avg a = sum a `div` length a -- the average value of the list
     apv = (avg . findPeaks . map fromIntegral) row
 in ((avg . map fromIntegral) row)*apv
foldDataTile :: BS.ByteString -> Int -> Int
foldDataTile s w =
 let image = toLists $ fromList (BS.length s `div` w) w (BS.unpack s)
     in sum $ map contextSensitiveCoeffOf image

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



  --     csm = foldl (\a b -> (fromIntegral b) `xor` (fromIntegral a)) 0xf row  -- the controlsum of the list



     