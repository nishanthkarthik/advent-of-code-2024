{-# LANGUAGE MagicHash #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Num (integerSizeInBase#)
import GHC.Word (Word(W#))
import Control.Monad
import Control.Applicative

import Commons

digitCount :: Integer -> Word
digitCount i = W# (integerSizeInBase# (let W# x = 10 in x) i)

step :: Integer -> Integer -> M.Map Integer Integer
step 0 c = M.singleton 1 c
step n c
    | even d = M.fromListWith (+) [(n `mod` (10 ^ div d 2), c), (n `div` (10 ^ div d 2), c)]
    | otherwise = M.singleton (n * 2024) c
    where d = digitCount n

turn :: M.Map Integer Integer -> M.Map Integer Integer
turn m = M.unionsWith (+) nextStep
    where nextStep = map (uncurry step) $ M.assocs m

main :: IO ()
main = do
    i <- inp (At.sepBy1 At.decimal At.space)
    let start = M.fromList $ map (,1) i
        score = sum . M.elems
    print $ score (iterate turn start !! 25)
    print $ score (iterate turn start !! 75)
