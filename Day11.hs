{-# LANGUAGE MagicHash #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import GHC.Num (integerSizeInBase#)
import GHC.Word

import Commons

digitCount :: Integer -> Word
digitCount i = W# (integerSizeInBase# (let W# x = 10 in x) i)

step :: Integer -> S.Set Integer
step 0 = S.singleton 1
step n
    | even d = S.fromList [n `mod` (10 ^ div d 2), n `div` (10 ^ div d 2)]
    | otherwise = S.singleton (n * 2024)
    where d = digitCount n

main :: IO ()
main = do
    i <- inp (At.sepBy1 At.decimal At.space)
    let stepG = foldr (\cu ac -> step cu `S.union` ac) S.empty
    -- TODO cool! this reaches steady state at 3811 elements
    mapM_ (print . S.size) $ take 150 $ iterate stepG $ S.fromList i
