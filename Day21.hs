import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.List (intersperse, lookup, group, nub)
import Data.Char (isHexDigit, digitToInt)
import Data.Tuple (swap)
import Data.Maybe (fromJust)
import Debug.Trace

import Commons

dirChr :: V2i -> Char
dirChr (V2 0 0) = 'A'
dirChr (V2 1 0) = 'v'
dirChr (V2 (-1) 0) = '^'
dirChr (V2 0 1) = '>'
dirChr (V2 0 (-1)) = '<'

numPad :: M.Map Int V2i
numPad = M.fromList [(7, V2 0 0), (8, V2 0 1), (9, V2 0 2),
                     (4, V2 1 0), (5, V2 1 1), (6, V2 1 2),
                     (1, V2 2 0), (2, V2 2 1), (3, V2 2 2),
                                  (0, V2 3 1), (10, V2 3 2)]

dirPad :: M.Map V2i V2i
dirPad = M.fromList [(V2 (-1) 0, V2 0 1), (V2 0 0, V2 0 2),
                     (V2 0 (-1), V2 1 0), (V2 1 0, V2 1 1), (V2 0 1, V2 1 2)]

steps :: Ord a => M.Map a V2i -> a -> a -> [[V2i]]
steps mm fr to = let V2 m n = mm M.! to - mm M.! fr
                     rng i = if i >= 0 then [1..i] else [i..(-1)]
                     dy = [V2 0 (signum n) | j <- rng n]
                     dx = [V2 (signum m) 0 | i <- rng m]
                     valid xs = all (\i -> i `elem` M.elems mm) $ scanl (+) (mm M.! fr) xs
                 in nub $ filter valid [dx ++ dy ++ [0], dy ++ dx ++ [0]]

keySeq :: Ord a => M.Map a V2i -> [a] -> [[V2i]]
keySeq m xs = map concat $ sequence $ do
    (fr, to) <- zip xs $ tail xs
    return (steps m fr to)

solve :: [Int] -> Int -> Int
solve xs times = let s1 = keySeq numPad (10 : xs)
                     sn = iterate (concatMap (keySeq dirPad . (0 :))) s1 !! times
                 in minimum $ map length sn

main :: IO ()
main = do
    rows <- inp (At.sepBy1 (At.many1 (digitToInt <$> At.satisfy isHexDigit)) At.endOfLine)
    nums <- inp (At.sepBy1 (At.decimal <* At.char 'A') At.endOfLine)
    print $ sum $ zipWith (\r n -> solve r 2 * n) rows nums
    print $ zipWith (\r n -> solve r 25 * n) rows nums
    return ()
