import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.List (transpose, sort)

import Commons

data St = St {position::V2i, velocity::V2i} deriving Show

parser :: At.Parser [St]
parser = do
    let c = V2 <$> (At.signed At.decimal <* At.char ',') <*> At.signed At.decimal
    let row = St <$> (At.string "p=" *> c) <*> (At.string " v=" *> c)
    At.sepBy1 row At.endOfLine

nX = 101
nY = 103

step :: Int -> St -> St
step i (St p v) = St (V2 (mod x nX) (mod y nY)) v
    where V2 x y = p + V2 i i * v + V2 i i * V2 nX nY

main :: IO ()
main = do
    pvs <- inp parser
    let pos n = map (position . step n) pvs
        (mX, mY) = (div nX 2, div nY 2)
        (iX, iY) = (\(V2 a _) -> a, \(V2 _ b) -> b)
        filts = [fromEnum . liftA2 (&&) l r | l <- [(< mX) . iX, (> mX) . iX], r <- [(< mY) . iY, (> mY) . iY]]
        solve = product . map sum . transpose . map (\p -> map ($ p) filts) . pos
    print $ solve 100

    -- https://github.com/glguy/advent/blob/main/solutions/src/2024/14.hs#L40
    let overlapScore = M.size . M.filter (== 1) . M.fromListWith (+) . map (,1)
        solve2 = filter (\i -> overlapScore (pos i) == length pvs) [1..10000]
    print solve2
