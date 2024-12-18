{-# LANGUAGE LambdaCase #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Control.Monad
import Control.Applicative
import Data.Char (digitToInt, intToDigit)
import Data.Maybe (isNothing, fromJust, maybe)
import Data.Foldable (toList)
import Debug.Trace

import Commons

data Block = Free Int | Used Int Int deriving (Eq, Show)

prettyShow :: Block -> String
prettyShow (Free n) = replicate n '.'
prettyShow (Used i n) = replicate n (intToDigit i)

prettyBlocks :: [Block] -> String
prettyBlocks = concatMap prettyShow

merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = x : y : merge xs ys

compact :: Q.Seq Block -> Maybe (Q.Seq Block)
compact Q.Empty = Just Q.Empty
compact (xs Q.:|> Free f) = compact xs
compact l@(xs Q.:|> Used i n) = if isNothing firstFree then Nothing else Just updated
    where firstFree = Q.findIndexL (\case Free _ -> True; _ -> False) xs
          freeIdx = fromJust firstFree
          (Free f) = Q.index xs freeIdx
          (l, rh Q.:<| rs) = Q.splitAt freeIdx xs
          updated = l Q.>< Q.fromList freeRepl Q.>< rs Q.>< Q.fromList usedRepl
          (freeRepl, usedRepl) = if
                                    | f == n -> ([Used i n], [])
                                    | f < n -> ([Used i f], [Used i (n - f)])
                                    | f > n -> ([Used i n, Free (f - n)], [])

repeatUntil :: (a -> Maybe a) -> a -> a
repeatUntil f a = go a
    where go a = maybe a go (f a)

fileCompact :: (Q.Seq Block, Int) -> Q.Seq Block
fileCompact (q, i) = if isNothing freeBlockIdx || (freeBlockI > usedI) then q else compacted
    where Just usedI = Q.findIndexR (\case Used i' _ | i' == i -> True; _ -> False) q
          Used _ nu = Q.index q usedI
          freeBlockIdx = Q.findIndexL (\case Free nf | nf >= nu -> True; _ -> False) q
          freeBlockI = fromJust freeBlockIdx
          Free nf = Q.index q freeBlockI
          filled = Q.insertAt usedI (Free nu) (Q.deleteAt usedI q)
          updated = Q.insertAt freeBlockI (Used i nu) (Q.deleteAt freeBlockI filled)
          compacted = if nf == nu then updated else Q.insertAt (freeBlockI + 1) (Free (nf - nu)) updated

main :: IO ()
main = do
    let blockTypes = merge (map Used [0..]) (repeat Free)
    d <- Q.fromList . zipWith ($) blockTypes <$> inp (At.many1 (digitToInt <$> At.digit))
    let cycles = repeatUntil compact d
        solve q = sum $ zipWith (*) (concatMap (\case Free n -> replicate n 0; Used i n -> replicate n i) q) [0..]
        s1 = solve cycles
    print s1

    let usedCount = (Q.length d + 1) `div` 2
        q2 = foldl (curry fileCompact) d [usedCount-1,usedCount-2..0]
        s2 = solve q2
    print s2
