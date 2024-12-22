import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.Maybe (isNothing, catMaybes)
import Control.Parallel.Strategies
import Debug.Trace

import Commons
import Algorithms

main :: IO ()
main = do
    blocks <- inp (At.sepBy1 (V2 <$> (At.decimal <* At.char ',') <*> At.decimal) At.endOfLine)
    let n = 70
        emptyGrid = M.fromList [(V2 i j, '.') | i <- [0..n], j <- [0..n]]
        fallen = 1024
        dirs = [V2 0 1, V2 1 0, V2 0 (-1), V2 (-1) 0]
        neighbor g v = [(v + d, Dist 1) | d <- dirs, M.member (v + d) g, g M.! (v + d) == '.']
        grid used = M.union (M.fromList $ map (,'#') (take used blocks)) emptyGrid :: M.Map V2i Char
        cost g = shortestPath (neighbor g) 0
        end = V2 n n
    print $ cost (grid fallen) M.! end

    let ends i = if isNothing endDist || endDist == Just Inf then Just (i - 1) else Nothing
            where run = cost (grid i) `using` rpar
                  endDist = M.lookup end run
        costs = parMap rpar ends [fallen..length blocks]
    print $ (blocks !!) $ head $ catMaybes costs
