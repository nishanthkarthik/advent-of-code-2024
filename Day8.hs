import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.List (groupBy, tails, sortOn)
import Data.Function (on)
import Data.Bifunctor (bimap)
import Data.Ix (inRange)

import Commons

antinode :: V2i -> V2i -> [V2i]
antinode a b = [a - del, b + del]
    where del = b - a

antinodes :: (V2i, V2i) -> V2i -> V2i -> [V2i]
antinodes dim a b = inGrid r1 ++ inGrid r2
    where r1 = iterate (del +) b
          r2 = iterate (subtract del) a
          del = b - a
          inGrid = takeWhile (inRange dim)

main :: IO ()
main = do
    g <- inp (parseGrid id)
    let towers = groupBy ((==) `on` snd) $ sortOn snd $ M.assocs $ M.filter (/= '.') g
        pairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]
        dims = bimap fst fst (M.findMin g, M.findMax g)
        cells fn = S.fromList [a | tow <- towers,
                                p <- pairs tow,
                                a <- uncurry fn (bimap fst fst p),
                                M.member a g]
    print $ S.size (cells antinode)
    print $ S.size (cells (antinodes dims))
    return ()
