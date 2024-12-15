import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import Data.List (sort, group)
import Data.Maybe (fromMaybe)

import Commons

parser :: At.Parser [(Int,Int)]
parser = At.sepBy1 ((,) <$> (At.decimal <* At.skipSpace) <*> At.decimal) At.endOfLine

main :: IO ()
main = do
    i <- inp parser

    let solve1 = sum $ map abs $ zipWith (-) (sort $ map fst i) (sort $ map snd i)
    print solve1

    let freq = M.fromList $ map (\i -> (head i, length i)) $ group $ sort $ map snd i
    let solve2 = sum $ map ((\i -> i * fromMaybe 0 (M.lookup i freq)) . fst) i
    print solve2

    return ()
