import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M

import Commons

type Idx = V2i
type Grid = M.Map (V2 Int) Char

parse :: At.Parser Grid
parse = do
    let cell = At.satisfy $ At.inClass "XMAS"
    let row = zip [0..] <$> At.many1 cell
    rows <- zip [0..] <$> At.sepBy1 row At.endOfLine
    let grid = [(V2 i j, c) | (i, cs) <- rows, (j, c) <- cs]
    return $ M.fromList grid

charVec :: Idx -> Idx -> Grid -> [Char]
charVec o d m = map (m M.!) valid
    where idxs = iterate (d +) o
          valid = takeWhile (`M.member` m) idxs

main :: IO ()
main = do
    i <- inp parse
    let deltas = [V2 i j | i <- [-1..1], j <- [-1..1]]
    let hits = length $ filter ((== "XMAS") . take 4) [charVec k d i | k <- M.keys i, d <- deltas]
    print hits

    let corners = [V2 i j | i <- [-1, 1], j <- [-1, 1]]
    let hits = filter ((== 4) . length) [[i M.! (k + c) | c <- corners,
                M.member (k + c) i, (i M.! (k + c)) `elem` ['M', 'S']] | k <- M.keys i, i M.! k == 'A']
    let crosses = ["MSMS", "SMSM", "SSMM", "MMSS"]
    print $ length $ filter (`elem` crosses) hits
    return ()
