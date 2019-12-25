{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
module Path
  (shortestPath)
where

import Protolude
import Utils

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import qualified Data.Set as Set
import Data.Set (Set)

import Data.Foldable (foldl')

import qualified Data.PQueue.Prio.Min as Queue

-- | find the shortest path in a graph
dijkstra ::
  forall v w. (Hashable v, Ord v, Ord w, Num w)
  => (v -> [(w, v)]) -- ^ transition function frow vertex *v* to new vertices associated with weight *w*.
  -> (w -> w -> w) -- ^ weight combination function, usually (+) for distances
  -> v -- ^ starting vertex
  -> v -- ^ end vertex
  -> HashMap v (w, v) -- ^ associate a vertex *v* with its weight from the starting vertex and its previous vertex
dijkstra getNext combineWeight start end = go (Queue.singleton 0 start) HashMap.empty Set.empty
  where
    go :: Queue.MinPQueue w v -> HashMap v (w, v) -> Set v -> HashMap v (w, v)
    go queue prevs done =
      case Queue.minViewWithKey queue of
        Nothing -> prevs
        Just ((w, currentPoint), queue')
          | currentPoint == end -> prevs
          | currentPoint `Set.member` done -> go queue' prevs done
          | otherwise ->
            let
              nexts = getNext currentPoint
              nextPriority = map (\(w', v) -> (w' `combineWeight` w, v)) nexts

              -- update queue
              queue'' = foldl' (\acc (k, a) -> Queue.insert k a acc) queue' nextPriority
              -- update prevs
              upPrevs = HashMap.fromList (map (\(weight, v) -> (v, (weight, currentPoint))) nextPriority)

              fUnion p0@(weight, _) p1@(weight', _)
                | weight <= weight' = p0
                | otherwise = p1
              in go queue'' (HashMap.unionWith fUnion prevs upPrevs) (Set.insert currentPoint done)

-- | find the shortest path in a graph
shortestPath ::
  forall v w. (Hashable v, Show w, Ord v, Ord w, Num w)
  => (v -> [(w, v)]) -- ^ transition function frow vertex *v* to new vertices associated with weight *w*.
  -> (w -> w -> w) -- ^ weight combination function, usually (+) for distances
  -> v -- ^ starting vertex
  -> v -- ^ ending vertex
  -> Maybe (w, [v]) -- ^ the list of vertices of the path associated with the weight
shortestPath getNext combineWeight start end = let
  d = dijkstra getNext combineWeight start end
  in buildPath start end d

buildPath ::
  (Hashable v, Show w, Ord v, Ord w, Num w)
  => v -- ^ starting vertex
  -> v -- ^ ending vertex
  -> HashMap v (w, v) -- ^ result of *dijkstra*
  -> Maybe (w, [v]) -- ^ resulting path with its weight
buildPath start end d
  | start == end = Just (0, [])
  | otherwise = case HashMap.lookup end d of
  Nothing -> Nothing
  Just (w, _prev) -> Just (w, go end [])
    where
      go current acc
        | current == start = acc
        | Just (_, prev) <- HashMap.lookup current d = go prev (current:acc)
        | otherwise = error "WTF buildPath"
