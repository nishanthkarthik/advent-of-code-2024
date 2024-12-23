import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.Bits
import Data.Containers.ListUtils
import Control.Parallel.Strategies

import Commons
import Algorithms

step :: Int -> Int
step n = let pr a = a .&. ((1 .<<. 24) - 1)
             n0 = pr (n .^. (n .<<. 6))
             n1 = pr (n0 .^. (n0 .>>. 5))
             n2 = pr (n1 .^. (n1 .<<. 11))
         in n2

ch :: Int -> [([Int], Int)]
ch n = zip dels $ drop 4 s
    where s = map (`mod` 10) $ iterate step n
          del = zipWith (-) (tail s) s
          dels = [take 4 (drop n del) | n <- [0..]]

main :: IO ()
main = do
    ns <- inp (At.sepBy1 At.decimal At.endOfLine)
    print $ sum $ map ((!! 2000) . iterate step) ns
    let combos a = M.fromListWith (const id) $ take 1996 $ ch a
        allCombos = M.unionsWith (+) $ parMap rpar combos ns
        bananas = maximum $ map snd $ M.assocs allCombos
    print bananas
