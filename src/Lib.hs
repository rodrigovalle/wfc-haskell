module Lib
    ( loadBitmap
    , findPatterns
    ) where

import Codec.BMP
import qualified Counter as C
import qualified Data.ByteString as BS
import Data.Hashable
import qualified Data.Vector as V
import Data.Word

type Pixel = (Word8, Word8, Word8)

type Bitmap = V.Vector (V.Vector Pixel)

data BitmapMeta = BitmapMeta
    { width :: Int
    , height :: Int
    , pixels :: Bitmap -- better performance with repa arrays?
    } deriving (Show)

-- | Make vectors hashable.
instance (Hashable a) => Hashable (V.Vector a) where
    hashWithSalt salt = hashWithSalt salt . V.toList

-- | Get the BMP file as a list of 8bit RGB color values.
loadBitmap :: String -> IO BitmapMeta
loadBitmap filename = do
    Right bmp <- readBMP filename
    let rgba = BS.unpack $ unpackBMPToRGBA32 bmp
        (width, height) = bmpDimensions bmp
        bitmap = constructBitmap width height (toPixels rgba)
    return bitmap

-- | Bundle a list of bytes into pixels, discarding the alpha channel.
toPixels :: [Word8] -> [Pixel]
toPixels [] = []
toPixels (r:g:b:_a:xs) = (r, g, b) : toPixels xs

-- | Convert a list of bytes from BMP file into a 2D list of 8 bit pixel values.
constructBitmap :: Int -> Int -> [Pixel] -> BitmapMeta
constructBitmap width height = BitmapMeta width height . V.unfoldr go
  where
    go [] = Nothing
    go list =
        let (head, tail) = splitAt width list
         in Just (V.fromList head, tail)

-- | Count the frequencies of unique NxM subarrays of a bitmap.
-- Possible optimization: use V.unsafeSlice
findPatterns :: Int -> Int -> BitmapMeta -> C.Counter Bitmap
findPatterns n m (BitmapMeta width height pixels) =
    foldr C.insert C.empty allSubarrays
  where
    allSubarrays =
        let is = [0 .. (height - n)]  -- rows
            js = [0 .. (width - m)]   -- columns
            subArray i j = V.map (V.slice j m) . V.slice i n
         in [subArray i j pixels | i <- is, j <- js]
