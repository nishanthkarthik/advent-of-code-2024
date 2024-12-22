import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.List (sort)
import Debug.Trace

import Commons
import Algorithms

dirs :: [V2i]
dirs = [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)]

pathFromWeights :: M.Map V2i Dist -> V2i -> S.Set V2i
pathFromWeights w cur
    | w M.! cur == Dist 0 = S.singleton cur
    | otherwise = let neighbors = filter (`M.member` w) $ map (cur +) dirs
                      lowestCost = minimum $ map (w M.!) neighbors
                      next = head $ filter ((== lowestCost) . (w M.!)) neighbors
                  in S.insert cur (pathFromWeights w next)

matchWindow :: M.Map V2i Dist -> Int -> V2i -> [Int]
matchWindow w len p = let wrange = [-len,-len+1..len]
                          del (Dist a) (Dist b) = a - b
                        in [del (w M.! (p + V2 i j)) (w M.! p) - (abs i + abs j) | i <- wrange, j <- wrange,
                                abs i + abs j <= len, M.member (p + V2 i j) w,
                                del (w M.! (p + V2 i j)) (w M.! p) > (abs i + abs j)]

main :: IO ()
main = do
    inputGrid <- inp (parseGrid id)
    let findPos c = head $ M.keys $ M.filter (== c) inputGrid
        (begin, end) = (findPos 'S', findPos 'E')
        g = M.union (M.fromList [(begin, '.'), (end, '.')]) inputGrid
        getNeighbor v = map (,Dist 1) $ filter (\i -> M.member i g && g M.! i == '.') $ map (v +) dirs
        weights = shortestPath getNeighbor begin
        path = pathFromWeights weights end
        solve n = sum $ map snd $ filter ((>= 100) . fst) $ M.assocs $ counter $ concatMap (matchWindow weights n) path
    print $ solve 2
    print $ solve 20
    return ()
