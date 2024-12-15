import qualified Data.Attoparsec.Text as At
import Control.Applicative ((<|>))
import Data.Maybe (maybeToList)
import Control.Monad.State

import Commons

data Instr = Mul Int Int | Turn Bool deriving (Eq, Show)

parse1 :: At.Parser [Instr]
parse1 = do
    let mul = Mul <$> (At.string "mul(" *> At.decimal <* ",") <*> (At.decimal <* ")")
    let doMul = Turn True <$ At.string "do()"
    let dontMul = Turn False <$ At.string "don't()"
    let alt = mul <|> dontMul <|> doMul <|> (At.anyChar *> alt)
    At.many1 alt

stepInst :: Instr -> State (Bool, Int) ()
stepInst (Turn t) = do
    (_, n) <- get
    put (t, n)
stepInst m = do
    (on, n) <- get
    put (on, if on then n + val m else n)

val :: Instr -> Int
val (Turn _) = 0
val (Mul a b) = a * b

main :: IO ()
main = do
    i <- inp parse1
    print $ sum $ map val i
    print $ execState (mapM_ stepInst i) (True, 0)
