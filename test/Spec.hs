import Lib

main :: IO ()
main = do
    bitmap <- loadBitmap "Dungeon.bmp"
    print $ findPatterns 4 4 bitmap
    return ()
