import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Debug.Trace

import Commons
import Algorithms

paths :: M.Map (V2i, V2i) Dist -> (V2i, V2i) -> [[(V2i, V2i)]]
paths m c@(cp, cd@(V2 dx dy))
    | m M.! c == Dist 0 = [[c]]
    | otherwise = concatMap (map (c :) . paths m) used
    where nexts = filter (`M.member` m) [(cp - cd, cd), (cp, V2 (-dy) dx), (cp, V2 dy (-dx))]
          -- any neighbor that's used must have a cost directly derivable from the current cell
          used = filter (\n@(np, _) ->
            let (Dist d1) = m M.! c
                (Dist d2) = m M.! n
            in d1 == d2 + if np /= cp then 1 else 1000) nexts

main :: IO ()
main = do
    g <- inp $ parseGrid id
    let findIdx c = S.findMin . M.keysSet . M.filter (== c)
        startPos = findIdx 'S' g
        endPos = findIdx 'E' g
        startGrid = M.union (M.fromList [(startPos, '.'), (endPos, '.')]) g
        open p = M.member p startGrid && startGrid M.! p == '.'
        neighbors (p, d@(V2 dx dy)) = filter (open . fst . fst) [((p + d, d), Dist 1),
                                       ((p, V2 (-dy) dx), Dist 1000), ((p, V2 dy (-dx)), Dist 1000)]
        weights = shortestPath neighbors (startPos, V2 0 1)
        smallestWeights = filter ((== endPos) . fst . fst) $ M.assocs weights
        solve1 = minimum $ map snd smallestWeights
        ends = filter ((== solve1) . snd) smallestWeights
    print solve1

    let solve2 = S.fromList $ map fst $ concat $ concatMap (paths weights . fst) ends
    print $ S.size solve2
