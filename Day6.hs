import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad
import Control.Applicative
import Data.List (unfoldr)
import Control.Parallel.Strategies

import Commons

type Grid = M.Map V2i Char

charVec :: V2i -> V2i -> Grid -> [Char]
charVec o d m = map (m M.!) valid
    where idxs = iterate (d +) o
          valid = takeWhile (`M.member` m) idxs

data StopStep = ExitGrid | EnterLoop deriving (Eq, Show)

stepWithState :: Grid -> (V2i, V2i) -> State (S.Set (V2i, V2i)) (V2i, V2i, StopStep)
stepWithState g (p, d) = do
    v <- get
    let takeRight (V2 i j) = V2 j (-i)
        newCell = S.notMember (p, d) v
    if
        | Just '#' == g M.!? (p + d) && newCell -> do
            modify (S.insert (p, d))
            stepWithState g (p, takeRight d)
        | Just '.' == g M.!? (p + d) && newCell -> do
            modify (S.insert (p, d))
            stepWithState g (p + d, d)
        | otherwise -> do
            when newCell (modify (S.insert (p, d)))
            pure (p, d, if not newCell then EnterLoop else ExitGrid)

dir :: Char -> V2i
dir '<' = V2 0 (-1)
dir '>' = V2 0 1
dir '^' = V2 (-1) 0
dir 'v' = V2 1 0
dir _ = error "dir"

main :: IO ()
main = do
    g <- inp $ parseGrid id
    let (final, steps) = runState (stepWithState startGrid (start, vel)) S.empty
        (start, vel) = head [(k, dir v) | (k,v) <- M.assocs g, v `elem` ("^v<>" :: String)]
        startGrid = M.insert start '.' g
        stepCount = S.size $ S.map fst steps
    print final
    print stepCount

    let blocks = map (\p -> M.insert p '#' startGrid) $ S.toList $ S.map fst steps
        traversals = parMap rpar (\m -> runState (stepWithState m (start, vel)) S.empty) blocks
        loops = filter (\((_,_,stop), b) -> stop == EnterLoop) traversals
    print $ length loops
