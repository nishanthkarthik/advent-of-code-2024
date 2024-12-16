import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.List (tails, partition, intersect, sortBy)

import Commons

parser :: At.Parser (S.Set (Int, Int), [[Int]])
parser = do
    let edge = (,) <$> (At.decimal <* At.string "|") <*> At.decimal
    edges <- At.many1 (edge <* At.endOfLine) <* At.endOfLine
    logs <- At.sepBy1 (At.sepBy1 At.decimal (At.string ",")) At.endOfLine
    return (S.fromList edges, logs)

main :: IO ()
main = do
    (edges, logs) <- inp parser
    let (goodLogs, badLogs) = partition (not . any (`S.member` revEdges) . pairs) logs
        revEdges = S.map (uncurry (flip (,))) edges
        pairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]
        solve = sum . map (\i -> i !! (length i `div` 2))
    print $ solve goodLogs

    let fixedBadLogs = map (sortBy (\i j -> if (j, i) `S.member` edges then GT else EQ)) badLogs
    print $ solve fixedBadLogs
