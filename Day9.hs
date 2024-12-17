{-# LANGUAGE LambdaCase #-}

import qualified Data.Attoparsec.Text as At
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Control.Monad
import Control.Applicative
import Data.Char (digitToInt)
import Data.Maybe (isNothing, fromJust, maybe)

import Commons

data Block = Free Int | Used Int Int deriving (Eq, Show)

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

main :: IO ()
main = do
    let blockTypes = merge (map Used [0..]) (repeat Free)
    d <- Q.fromList . zipWith ($) blockTypes <$> inp (At.many1 (digitToInt <$> At.digit))
    let cycles = repeatUntil compact d
        expandedBlocks = concatMap (\case Free _ -> []; Used i n -> replicate n i) cycles
        s1 = sum $ zipWith (*) expandedBlocks [0..]
    print s1
    return ()
