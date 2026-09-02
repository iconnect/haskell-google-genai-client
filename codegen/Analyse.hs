{-# LANGUAGE OverloadedStrings #-}

-- | Turns a parsed Discovery document into a language-neutral emission plan:
-- which schemas to emit, their direction, field typing, naming, endpoints.
module Analyse
  ( Direction (..)
  , isRequired
  , isOutputOnly
  , refsOf
  , closure
  , directions
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Discovery

data Direction = ResponseOnly | Bidirectional
  deriving (Show, Eq)

-- | Google marks fields with leading sentences such as @Required.@,
-- @Optional.@, @Output only.@, @Immutable.@, in any order. Only the run of
-- marker sentences at the very start counts, so prose mentioning the words
-- later does not.
hasMarker :: Text -> Property -> Bool
hasMarker marker p = marker `elem` leading
  where
    leading = takeWhile (`elem` markers) (sentences (propDescription p))
    markers = ["Required.", "Optional.", "Output only.", "Immutable.", "Identifier.", "Input only."]
    sentences = map (<> ".") . filter (not . T.null) . map T.strip . T.splitOn "." . T.take 80

isRequired, isOutputOnly :: Property -> Bool
isRequired = hasMarker "Required."
isOutputOnly = hasMarker "Output only."

-- | Schema ids referenced by a property, directly or through items/map values.
refsOf :: Property -> [Text]
refsOf p =
  maybe [] pure (propRef p)
    ++ maybe [] refsOf (propItems p)
    ++ maybe [] refsOf (propAdditional p)

-- | Transitive closure over @$ref@ edges, following only properties that
-- satisfy the predicate.
closure :: Map Text Schema -> (Property -> Bool) -> Set Text -> Set Text
closure schemas keep = go
  where
    go seen =
      let next =
            Set.fromList
              [ r
              | sid <- Set.toList seen
              , Just s <- [Map.lookup sid schemas]
              , p <- Map.elems (schemaProperties s)
              , keep p
              , r <- refsOf p
              ]
          seen' = Set.union seen next
       in if seen' == seen then seen else go seen'

-- | Every emitted schema with its direction. A schema is 'ResponseOnly' when
-- it is reachable from some method response and not reachable from any method
-- request through non-@Output only.@ properties.
directions :: Doc -> [Method] -> Map Text Direction
directions doc methods =
  Map.fromSet classify emitted
  where
    schemas = docSchemas doc
    respSeed = Set.fromList (mapMaybe methodResponse methods)
    reqSeed = Set.fromList (mapMaybe methodRequest methods)
    reachResp = closure schemas (const True) respSeed
    reachReq = closure schemas (not . isOutputOnly) reqSeed
    emitted = closure schemas (const True) (Set.union respSeed reqSeed)
    classify s
      | Set.member s reachResp && not (Set.member s reachReq) = ResponseOnly
      | otherwise = Bidirectional
