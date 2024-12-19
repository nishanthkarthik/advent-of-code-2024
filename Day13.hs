import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.Ratio
import Data.Maybe

import Commons

data Machine = Machine V2i V2i V2i deriving Show

parser :: At.Parser [Machine]
parser = do
    let btn = V2 <$> ((At.string "Button " >> At.anyChar >> At.string ": X+") *> At.decimal)
                <*> (At.string ", Y+" *> At.decimal)
        prize = V2 <$> (At.string "Prize: X=" *> At.decimal) <*> (At.string ", Y=" *> At.decimal)
        eol = At.char '\n'
        machine = Machine <$> (btn <* eol) <*> (btn <* eol) <*> (prize <* eol)
    At.sepBy1 machine eol

linSolve :: V2i -> V2i -> V2i -> Maybe (V2 Rational)
linSolve (V2 p r) (V2 q s) (V2 m n)
    | q * r == p * s = Nothing
    | otherwise = Just $ V2 (k1 % d) (k2 % d)
    where k1 = fromIntegral (n * q - m * s)
          k2 = fromIntegral (m * r - p * n)
          d = fromIntegral (q * r - p * s)

main :: IO ()
main = do
    i <- inp parser
    let solns = mapMaybe (\(Machine a b p) -> linSolve a b p)
        costs = map (V2 3 1 *) . filter (\(V2 x y) -> denominator x == 1 && denominator y == 1) . solns
        solve = sum . map (\(V2 x y) -> numerator x + numerator y) . costs
    print $ solve i

    let offset = 10000000000000
        bump (Machine a b p) = Machine a b (p + offset)
    print $ solve $ map bump i
