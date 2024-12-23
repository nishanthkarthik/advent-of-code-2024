import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Graph as G
import Control.Monad
import Control.Applicative
import Data.Char (isAlpha, chr, ord)
import Data.Bifunctor

import Commons
import Data.Tuple (swap)

enc :: String -> Int
enc xs = let [a, b] = xs in ord a * 256 + ord b

dec :: Int -> String
dec n = map chr [div n 256, mod n 256]

main :: IO ()
main = do
    let name = At.many1 $ At.satisfy isAlpha
    conns <- inp (At.sepBy1 ((,) <$> (name <* At.char '-') <*> name) At.endOfLine)
    let edges = M.fromListWith IS.union $ concat [[(enc i, IS.singleton $ enc j), (enc j, IS.singleton $ enc i)] | (i,j) <- conns]
        tstart n = chr (n `div` 256) == 't'
        triplets = S.fromList [IS.fromList [k, v, tr]
                | (k, vs) <- M.assocs edges, v <- IS.toList vs, IS.size (IS.intersection vs (edges M.! v)) > 0,
                    tr <- IS.toList $ IS.intersection vs (edges M.! v)]
        refine :: S.Set IS.IntSet -> S.Set IS.IntSet
        refine ss = S.fromList [IS.insert v s | s <- S.toList ss,
                        (v, vs) <- M.assocs edges, s `IS.isSubsetOf` vs]
        decode = S.map (S.map dec)

    print $ S.size $ S.filter (any tstart . IS.toList) triplets
    mapM_ (print . map dec . IS.toList . S.findMin) $ takeWhile (\s -> S.size s > 0) $ iterate refine triplets
