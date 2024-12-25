import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Control.Monad
import Control.Applicative
import Data.Function (on)
import Debug.Trace
import Data.Bits
import Data.Char
import Control.Parallel.Strategies
import Control.Exception

import Commons
import Data.List
import Data.Maybe

type Gate = String
type EGate = Int

data Op = ZAnd EGate EGate | ZXor EGate EGate | ZOr EGate EGate deriving Show

encodeGate :: Gate -> EGate
encodeGate [a,b,c] = (ord a .<<. 16) .|. (ord b .<<. 8) .|. ord c

decodeGate :: EGate -> Gate
decodeGate n = [chr (n .>>. 16), chr (mod (n .>>. 8) 256), chr (mod n 256)]

fstEGate :: Char -> EGate -> Bool
fstEGate c g = (g .>>. 16) == ord c

parser :: At.Parser (IM.IntMap Bool, IM.IntMap Op)
parser = do
    let gate = encodeGate <$> At.count 3 (At.satisfy isAlphaNum)
        bool = (== '1') <$> At.digit
        val = (,) <$> (gate <* At.string ": ") <*> bool
        space = At.char ' '
        op gs g = do
            g1 <- gate <* space
            o <- g <$ At.string gs <* space
            g2 <- gate <* At.string " -> "
            g3 <- gate
            return (g3, o g1 g2)
        ops = At.choice [op "AND" ZAnd, op "OR" ZOr, op "XOR" ZXor]
    vals <- At.sepBy1 val At.endOfLine <* At.many1 At.endOfLine
    calcs <- At.sepBy1 ops At.endOfLine
    return (IM.fromList vals, IM.fromList calcs)

opOf :: (EGate -> Bool) -> Op -> Bool
opOf f (ZAnd a b) = f a && f b
opOf f (ZXor a b) = f a /= f b
opOf f (ZOr a b) = f a || f b

data CycleInEvalException = CycleInEvalException deriving Show
instance Exception CycleInEvalException

eval :: Int -> IM.IntMap Bool -> IM.IntMap Op -> EGate -> Bool
eval depth !vals !ops !gate
    | depth >= 1000 = throw CycleInEvalException
    | IM.member gate vals = vals IM.! gate
    | otherwise = opOf (eval (depth + 1) vals ops) (ops IM.! gate)

depOf :: Op -> [EGate]
depOf (ZAnd a b) = [a, b]
depOf (ZOr a b) = [a, b]
depOf (ZXor a b) = [a, b]

deps :: IM.IntMap Op -> EGate -> IS.IntSet
deps ops gate = IS.insert gate
        $ IS.unions [if IM.member g ops then deps ops g else IS.singleton g | g <- depOf (ops IM.! gate)]

asBin :: Int -> [Bool]
asBin = go
    where go 0 = [False]
          go 1 = [True]
          go i = (mod i 2 == 1) : go (div i 2)

main :: IO ()
main = do
    (vals, ops) <- inp parser
    let zs = [eval 0 vals ops k | k <- IM.keys ops, fstEGate 'z' k]
        asNum = foldr (\cu ac -> ac * 2 + fromEnum cu) 0
    print $ asNum zs

    let mops = IM.union (IM.unions [eswaps "vcv" "z13", eswaps "vwp" "z19", eswaps "mps" "z25", eswaps "cqm" "vjv"]) ops
        eswaps a b = let ea = encodeGate a
                         eb = encodeGate b
                     in swaps ea eb
        swaps a b = IM.fromList [(a, ops IM.! b), (b, ops IM.! a)]
    let cs c = [k | k <- allGates, fstEGate c k]
        allGates = IS.toList $ IS.union (IM.keysSet mops) (IM.keysSet vals)
        adder m a b = let xs = IM.fromList $ zip (cs 'x') (asBin a ++ repeat False)
                          ys = IM.fromList $ zip (cs 'y') (asBin b ++ repeat False)
                      in map (eval 0 (IM.unions [xs, ys]) m) (cs 'z')
        pp :: [Bool] -> String
        pp = map (intToDigit . fromEnum)
        zne a b = or (zipWith (/=) a b)
        expectne m a b = zne (adder m a b) (asBin (a + b))
        combos xs = [(l, r) | l <- xs, r <- xs, l < r]

    let allSwaps = combos $ filter (\i -> not (fstEGate 'y' i) && not (fstEGate 'x' i)) allGates
    print $ length allSwaps

    forM_ allSwaps $ \(a,b) -> do
        let m = IM.union (swaps a b) mops
        let guardedExpectNe a b = catch (evaluate (expectne m a b)) (\(i :: CycleInEvalException) -> return True)
        mm <- forM [0..45] $ \i -> do
            let checks = [(1 .<<. i, 0), (1 .<<. i, 1 .<<. i), (0, 1 .<<. i), (1 .<<. i, 3 .<<. i)]
            mms <- mapM (uncurry guardedExpectNe) checks
            return (if or mms then Just (i, (decodeGate a, decodeGate b)) else Nothing)
        let mmc = minimumBy (compare `on` fst) $ catMaybes mm
        unless (null $ catMaybes mm) $ when (fst mmc > 44) $ print mmc
