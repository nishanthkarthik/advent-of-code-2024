import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.Bifunctor
import GHC.Num

import Commons

parser :: At.Parser [(Integer,[Integer])]
parser = flip At.sepBy1 At.endOfLine $ do
    (,) <$> (At.decimal <* At.string ": ") <*> At.sepBy1 At.decimal (At.char ' ')

type Inst = (Integer -> Integer -> Integer)

check :: Integer -> Integer -> [(Inst, Integer)] -> Bool
check n cur [] = cur == n
check n cur ((i,x):xs)
    | cur > n = False
    | otherwise = check n (cur `i` x) xs

main :: IO ()
main = do
    ts <- inp parser
    let solve fns = [n | (n, th : tt) <- ts,
            any (check n th . flip zip tt) (replicateM (length tt) fns)]
        solve1 = [(*), (+)]
        solve2 = [(*), (+), concatInt]
        concatInt a b = toInteger (integerPow10 $ fromIntegral (1 + integerLogBase 10 b)) * a + b
        integerPow10 a = iterate (10 *) 1 !! a
    print $ sum $ solve solve1
    print $ sum $ solve solve2
