module Lib
    ( loadBitmap,
    ) where

import qualified Data.ByteString as BS
import qualified Data.Counter as Counter
import Codec.BMP
import Data.Word

type Pixel = (Word8, Word8, Word8)
type Bitmap = [[Pixel]] -- better performance with repa arrays?

loadBitmap :: String -> IO Bitmap
loadBitmap filename = do Right bmp <- readBMP filename
                         let rgba = BS.unpack $ unpackBMPToRGBA32 bmp
                             (width, height) = bmpDimensions bmp
                             bitmap = constructBitmap width rgba
                         return bitmap

-- Use a list comprehension instead?
-- Make it pointfree?
constructBitmap :: Int -> [Word8] -> Bitmap
constructBitmap width [] = []
constructBitmap width xs = go head : constructBitmap width tail
    where
          go [] = []
          go (r:g:b:_a:xs) = (r,g,b) : go xs
          go _ = error "corrupt BMP"
          (head, tail) = splitAt (width*4) xs
