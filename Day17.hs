import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Array.Unboxed as A
import Control.Monad
import Control.Applicative
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Data.Bits
import Data.List (tails)

import Commons

parser :: At.Parser ((Integer,Integer,Integer),[Int])
parser = do
    let reg = (At.string "Register " >> At.anyChar >> At.string ": ") *> At.decimal <* At.endOfLine
        prog = At.string "Program: " >> At.sepBy1 At.decimal (At.char ',')
    regs <- (,,) <$> reg <*> reg <*> reg
    (regs,) <$> (At.endOfLine *> prog)

data Inst = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv deriving (Eq, Ord, Enum, Show)

newtype Prog = Prog (A.UArray Int Int) deriving Show

data Store = Store { regA :: Integer, regB :: Integer, regC :: Integer, pc :: Int } deriving (Show)

eval :: Prog -> WriterT [Int] (State Store) ()
eval (Prog p) = do
    st <- get

    when (pc st <= snd (A.bounds p)) $ do
        let inst = toEnum $ p A.! pc st
            opnd = fromIntegral $ p A.! (1 + pc st)
            combo = if | 0 <= opnd && opnd <= 3 -> fromIntegral opnd
                       | opnd == 4 -> regA st
                       | opnd == 5 -> regB st
                       | opnd == 6 -> regC st
                       | opnd == 7 -> error "invalid combo"

        modify (\s -> s{pc = pc st + 2})

        case inst of
             Adv -> modify (\s -> s{regA = div (regA st) (2 ^ combo)})
             Bdv -> modify (\s -> s{regB = div (regA st) (2 ^ combo)})
             Cdv -> modify (\s -> s{regC = div (regA st) (2 ^ combo)})
             Bxl -> modify (\s -> s{regB = regB st .^. opnd})
             Bst -> modify (\s -> s{regB = mod combo 8})
             Bxc -> modify (\s -> s{regB = regB st .^. regC st})
             Out -> tell [fromInteger $ mod combo 8]
             Jnz -> unless (regA st == 0) $ modify (\s -> s{pc = fromInteger opnd})

        eval (Prog p)

-- https://old.reddit.com/r/haskell/comments/1hg39hy/advent_of_code_2024_day_17/m2hyhxk/
solve2 :: (Integer -> [Int]) -> [[Int]] -> Integer -> [Integer]
solve2 f [] n = [n]
solve2 f (x:xs) n = [nx | i <- [0..7], f (n * 8 + i) == x, nx <- solve2 f xs (n * 8 + i)]

main :: IO ()
main = do
    ((ra, rb, rc), prog) <- inp parser
    let startStore = Store ra rb rc 0
        simulate = snd . evalState (runWriterT $ eval $ Prog (A.listArray (0, length prog - 1) prog))
        s1 = simulate startStore
    print s1
    let bbFn i = simulate startStore{regA = i}
    print $ minimum $ solve2 bbFn (tail $ reverse $ tails prog) 0
