import qualified Data.Attoparsec.Text as At
import Data.Ix (inRange)

import Commons

parser :: At.Parser [[Int]]
parser = flip At.sepBy1 At.endOfLine $ do
    At.sepBy1 At.decimal $ At.char ' '

check1 :: [Int] -> Bool
check1 as = all (inRange (1, 3)) as || all (inRange (-3, -1)) as

pop1 :: [Int] -> [[Int]]
pop1 [] = []
pop1 (x:xs) = xs : map (x:) (pop1 xs)

diffs :: [Int] -> [Int]
diffs i = zipWith (-) i $ tail i

check2 :: [Int] -> Bool
check2 = any (check1 . diffs) . pop1

main :: IO ()
main = do
    i <- inp parser
    print $ length $ filter check1 $ map diffs i
    print $ length $ filter check2 i
