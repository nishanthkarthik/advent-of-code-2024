import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.List
import Debug.Trace

import Commons
import Algorithms

data Item = Lock [Int] | Key [Int] deriving (Show, Eq, Ord)

parser :: At.Parser (S.Set Item)
parser = do
    let c = At.satisfy $ At.inClass "#."
        r = At.count 5 c
        rs = do
            i <- At.count 7 (r <* At.endOfLine)
            let kind = if head (head i) == '#' then Lock else Key
            return $ kind $ map (pred . length . filter (== '#')) $ transpose i
    S.fromList <$> At.sepBy1 rs At.endOfLine

main :: IO ()
main = do
    items <- inp parser
    let locks = S.filter (\case (Lock _) -> True; _ -> False) items
        keys = S.filter (\case (Key _) -> True; _ -> False) items
    print $ sum [1 | (Lock l) <- S.toList locks, (Key k) <- S.toList keys, all (<= 5) $ zipWith (+) l k]
    return ()
