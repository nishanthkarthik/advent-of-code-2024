import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.Char (digitToInt)

import Commons

dirs = [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

dfs :: M.Map V2i Int -> V2i -> S.Set V2i
dfs g p = let
              val = g M.! p
              nexts = filter (\n -> g M.! n == val + 1) $ filter (`M.member` g) $ map (p +) dirs
          in if val == 9 then S.singleton p else foldl (\ac cu -> dfs g cu `S.union` ac) S.empty nexts

dfs2 :: M.Map V2i Int -> V2i -> Int
dfs2 g p = let
              val = g M.! p
              nexts = filter (\n -> g M.! n == val + 1) $ filter (`M.member` g) $ map (p +) dirs
           in if val == 9 then 1 else sum $ map (dfs2 g) nexts

main :: IO ()
main = do
    i <- inp $ parseGrid digitToInt
    let trailHeads = M.keys $ M.filter (== 0) i
        s1 = sum $ map (S.size . dfs i) trailHeads
        s2 = sum $ map (dfs2 i) trailHeads
    print s1
    print s2
