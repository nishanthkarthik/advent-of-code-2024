module Commons where

import qualified Data.Attoparsec.Text as At
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import qualified Data.Ix as I

import Data.Char (isSpace)
import System.Environment (getArgs)
import Text.Printf (printf)

inp :: At.Parser a -> IO a
inp parser = do
    input <- getArgs >>= TIO.readFile . head
    let result = At.eitherResult $ At.feed (At.parse parser input) mempty
    case result of
        Left l -> error l
        Right r -> return r

data V2 a = V2 a a deriving (Eq, Ord, Functor)

instance (Show a) => Show (V2 a) where
    show (V2 i j) = printf "(%s %s)" (show i) (show j)

instance Num a => Num (V2 a) where
    (+) (V2 a b) (V2 c d) = V2 (a + c) (b + d)
    (*) (V2 a b) (V2 c d) = V2 (a * c) (b * d)
    abs v = abs <$> v
    signum v = signum <$> v
    fromInteger a = fromInteger <$> V2 a a
    negate v = negate <$> v

instance (I.Ix a) => I.Ix (V2 a) where
    range (V2 a b, V2 c d) = let r = I.range ((a,b), (c,d)) in map (uncurry V2) r
    index (V2 a b, V2 c d) (V2 m n) = I.index ((a,b), (c,d)) (m,n)
    inRange (V2 a b, V2 c d) (V2 m n) = I.inRange ((a,b), (c,d)) (m,n)

type V2i = V2 Int

parseGrid :: (Char -> a) -> At.Parser (M.Map V2i a)
parseGrid cellFn = do
    let cell = At.satisfy (not . isSpace)
    let row = zip [0..] <$> At.many1 cell
    rows <- zip [0..] <$> At.sepBy1 row At.endOfLine
    let grid = [(V2 i j, cellFn c) | (i, cs) <- rows, (j, c) <- cs]
    return (M.fromList grid)

showGrid :: (a -> Char) -> M.Map V2i a -> String
showGrid f m = unlines [[f (m M.! V2 i j) | j <- [mny..mxy]] | i <- [mnx..mxx]]
    where ((V2 mnx mny, _), (V2 mxx mxy, _)) = (M.findMin m, M.findMax m)

inStr :: Char -> String -> Bool
inStr = elem
