import Lib
import qualified Counter as C

main :: IO ()
main = do
    bitmap <- loadBitmap "Dungeon.bmp"
    sample <- C.sample (findPatterns 4 4 bitmap)
    print sample
    return ()
