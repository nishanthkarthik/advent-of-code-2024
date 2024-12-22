import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.List (sortOn, splitAt, inits)
import Debug.Trace
import Control.Parallel.Strategies
import Data.MemoTrie

import Commons

parser :: At.Parser (S.Set String, [String])
parser = do
    let color = At.satisfy $ At.inClass "wubrg"
        colors = At.many1 color
    towels <- S.fromList <$> At.sepBy1 colors (At.string ", ")
    At.many1 At.endOfLine
    patterns <- At.sepBy1 colors At.endOfLine
    return (towels, patterns)

match2 :: S.Set String -> String -> Integer
match2 tow = memo go
    where go i
            | null i = 1
            | otherwise = sum $ map (match2 tow) chunks
            where chunks = map snd $ filter ((`S.member` tow) . fst) $ map (`splitAt` i) [1..maxMatchLen]
                  maxMatchLen = min (length i) $ maximum $ S.map length tow

main :: IO ()
main = do
    (tow, pat) <- inp parser
    let soln = parMap rpar (match2 tow) pat
    print $ length $ filter (> 0) soln
    print $ sum soln
    return ()
