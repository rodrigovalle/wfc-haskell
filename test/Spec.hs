import Lib

main :: IO ()
main = do bitmap <- loadBitmap "Dungeon.bmp"
          print bitmap
          return ()
