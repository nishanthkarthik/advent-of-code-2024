import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Debug.Trace

import Commons

dirs = [V2 (-1) 0, V2 0 1, V2 1 0, V2 0 (-1)]

floodFill :: M.Map V2i Char -> V2i -> S.Set V2i
floodFill g i = go S.empty $ S.singleton i
    where go :: S.Set V2i -> S.Set V2i -> S.Set V2i
          go vis cur
              | S.null cur = vis
              | otherwise = go (S.union vis cur) $ S.unions $ map nexts $ S.toList (S.difference cur vis)
          nexts c = S.fromList $ filter (\n -> g M.! n == g M.! i) $ filter (`M.member` g) $ map (c +) dirs

perimeter :: S.Set V2i -> Int
perimeter s = sum $ map ((4 -) . neighbors) $ S.toList s
    where neighbors p = length $ filter (`S.member` s) $ map (+ p) dirs

corner :: S.Set V2i -> Int
corner s = sum $ map pairs $ S.toList s
    where pairs p = sum $ map (pairOpen p) vecs
          vecs = zip dirs $ tail (cycle dirs)
          pairOpen p (a, b)
              | S.notMember (p + a) s && S.notMember (p + b) s = 1 -- outer corner
              | S.member (p + a) s && S.member (p + b) s && S.notMember (p + a + b) s = 1 -- inner corner
              | otherwise = 0

main :: IO ()
main = do
    g <- inp (parseGrid id)
    let paints = foldr (\cu ac -> if any (S.member cu) ac then ac else floodFill g cu : ac) [] $ reverse $ M.keys g
        areas = map S.size paints
        perimeters = map perimeter paints
        corners = map corner paints
        total1 = sum $ zipWith (*) areas perimeters
        total2 = sum $ zipWith (*) areas corners
    print total1
    print total2
