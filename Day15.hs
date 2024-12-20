import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Function (fix)
import Data.Bifunctor (second)
import Control.Monad
import Control.Applicative
import Debug.Trace

import Commons

type Grid = M.Map V2i Char

parser :: At.Parser (Grid, [Char])
parser = do
    grid <- parseGrid id <* At.many1 At.endOfLine
    dirs <- concat <$> At.sepBy1 (At.many1 (At.satisfy (At.inClass "^v<>"))) At.endOfLine
    return (grid, dirs)

step :: (Grid, V2i) -> V2i -> (Grid, V2i)
step (g, p) d
    | g M.! (p + d) == '#' = (g, p)
    | g M.! (p + d) == '.' = (g, p + d)
    | null boxes || null rest = (g, p)
    | g M.! head rest == '.' = (grid, head boxes)
    | otherwise = (g, p)
    where nexts = takeWhile (`M.member` g) $ tail $ iterate (+ d) p
          (boxes, rest) = span ((== 'O') . (g M.!)) nexts
          grid = M.insert (head boxes) '.' (M.insert (head rest) 'O' g)

mapDir :: Char -> V2i
mapDir 'v' = V2 1 0
mapDir '^' = V2 (-1) 0
mapDir '<' = V2 0 (-1)
mapDir '>' = V2 0 1

expand :: Grid -> Grid
expand = M.fromList . concatMap (\(p, c) -> zip [wide p, wide p + V2 0 1] (go c)) . M.assocs
    where go '#' = "##"
          go 'O' = "[]"
          go '.' = ".."
          go '@' = "@."
          wide (V2 x y) = V2 x (2 * y)

step2 :: (Grid, V2i) -> V2i -> (Grid, V2i)
step2 i@(g, p) d@(V2 x y)
    | g M.! (p + d) == '#' = (g, p)
    | g M.! (p + d) == '.' = (g, p + d)
    | otherwise = if x == 0 then step2y i d else step2x i d

step2y :: (Grid, V2i) -> V2i -> (Grid, V2i)
step2y (g, p) d
    | null boxes || null rest = (g, p)
    | g M.! head rest == '.' = (grid, head boxes)
    | otherwise = (g, p)
    where nexts = takeWhile (`M.member` g) $ tail $ iterate (+ d) p
          (boxes, rest) = span ((`inStr` "[]") . (g M.!)) nexts
          moveBoxes = map (\i -> (i + d, g M.! i)) boxes
          grid = M.union (M.fromList moveBoxes) (M.insert (head boxes) '.' g)


closure :: Eq a => (a -> a) -> a -> a
closure f = fix (\rec x -> if x == f x then x else rec (f x))

step2x :: (Grid, V2i) -> V2i -> (Grid, V2i)
step2x (g, p) d
    | all (open . (+ d)) $ S.toList boxes = (grid, p + d)
    | otherwise = (g, p)
    where starts = flood $ S.singleton (p + d)
          matching x = x + if g M.! x == '[' then V2 0 1 else V2 0 (-1)
          flood set = S.union (S.map matching set) set
          stepV set = S.union set $ flood $ S.filter (\i -> M.member i g && (g M.! i `inStr` "[]")) $ S.map (d +) set
          boxes = closure stepV starts
          open x = M.member x g && g M.! x `inStr` "[]."
          movedBoxes = map (\i -> (d + i, g M.! i)) $ S.toList boxes
          updates = [(S.findMin starts, '.'), (S.findMax starts, '.')]
          clearStarts = map (,'.') $ S.toList boxes
          grid = M.unions [M.fromList updates, M.fromList movedBoxes, M.fromList clearStarts, g]

main :: IO ()
main = do
    (g, ds) <- second (map mapDir) <$> inp parser
    let startPos = findStartPos g
        findStartPos = fst . M.findMin . M.filter (== '@')
        startGrid = M.insert startPos '.' g
        (finalGrid, _) = foldl step (startGrid, startPos) ds
        cost (V2 i j) = 100 * i + j
        solve1 = sum $ map cost $ M.keys $ M.filter (== 'O') finalGrid
    print solve1

    let wideGrid = expand g
        startPos = findStartPos wideGrid
        startGrid = M.insert startPos '.' wideGrid
        (finalGrid, _) = foldl step2 (startGrid, startPos) ds
        solve2 = sum $ map cost $ M.keys $ M.filter (== '[') finalGrid
    print solve2
