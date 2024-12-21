module Algorithms (shortestPath, Dist(Dist)) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

data Dist = Dist Int | Inf deriving (Eq, Ord, Show)

type GetNeighbors vert = vert -> [(vert, Dist)]
type DistMap vert = M.Map vert Dist
type PQueue vert = S.Set (Dist, vert)

shortestPath :: (Eq vert, Ord vert) => GetNeighbors vert -> vert -> DistMap vert
shortestPath getNeighbors start = go initialQueue initialDistMap S.empty
    where initialDistMap = M.singleton start (Dist 0)
          initialQueue = S.singleton (Dist 0, start)
          go qu dm vis = if S.null qu then dm else go qu' dm' vis'
            where ((Dist distU, vertU), restQu) = S.deleteFindMin qu
                  edges = filter (not . (`S.member` vis) . fst) $ getNeighbors vertU
                  edgeDists = map (\(v, Dist dv) -> min (fromMaybe Inf $ M.lookup v dm) (Dist (distU + dv))) edges
                  assocs = zip edgeDists $ map fst edges
                  dm' = M.union (M.fromList $ map swap assocs) dm
                  qu' = foldr S.insert restQu assocs
                  vis' = S.insert vertU vis
