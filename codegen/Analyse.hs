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
  , Presence (..)
  , Wire (..)
  , Field (..)
  , EnumDef (..)
  , TypeDef (..)
  , Rule (..)
  , overrides
  , upperFirst
  , lowerFirst
  , camel
  , hsFieldName
  , enumTypeName
  , enumCtor
  , fieldOf
  , typeDefOf
  ) where

import Data.Char (isDigit, toLower, toUpper)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
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

-- | How an absent JSON key is handled and how the field is encoded.
data Presence
  = -- | @o .: k@; always encoded; positional in the smart constructor.
    Required
  | -- | @Maybe@; @o .:? k@; encoded when @Just@.
    Optional
  | -- | @o .:? k .!= def@; always encoded. Carries the default expression.
    Defaulted Text
  | -- | list / Object / Map: @.!= mempty@; encoded only when non-empty.
    Mono
  deriving (Show, Eq)

data Wire = WPlain | WInt64
  deriving (Show, Eq)

data Field = Field
  { fJson :: Text
  -- ^ JSON key
  , fName :: Text
  -- ^ Haskell record field
  , fType :: Text
  -- ^ full Haskell type, e.g. @Maybe Text@, @[Part]@
  , fBase :: Text
  -- ^ type without @Maybe@, used by overrides
  , fWire :: Wire
  , fPresence :: Presence
  , fPositional :: Bool
  -- ^ argument of the smart constructor
  , fDoc :: Text
  }
  deriving (Show, Eq)

data EnumDef = EnumDef
  { enumName :: Text
  , enumDoc :: Text
  , enumCtors :: [(Text, Text, Text)]
  -- ^ (constructor, wire value, description); spec order
  }
  deriving (Show, Eq)

data TypeDef = TypeDef
  { tName :: Text
  , tDoc :: Text
  , tFields :: [Field]
  , tEnums :: [EnumDef]
  }
  deriving (Show, Eq)

-- | Per-field escape hatch, keyed by (schema id, property). Every entry must
-- carry a comment citing the real response that contradicts the default rules.
data Rule = ForceMaybe | ForceRequired | ForceDefault Text
  deriving (Show, Eq)

overrides :: Map (Text, Text) Rule
overrides = Map.empty

upperFirst, lowerFirst :: Text -> Text
upperFirst t = maybe t (\(c, r) -> T.cons (toUpper c) r) (T.uncons t)
lowerFirst t = maybe t (\(c, r) -> T.cons (toLower c) r) (T.uncons t)

-- | @display_name@ → @displayName@.
camel :: Text -> Text
camel t = case filter (not . T.null) (T.split (\c -> c == '_' || c == '-') t) of
  [] -> t
  (x : xs) -> x <> T.concat (map upperFirst xs)

hsFieldName :: Text -> Text -> Text
hsFieldName sid p = lowerFirst sid <> upperFirst (camel p)

enumTypeName :: Text -> Text -> Text
enumTypeName sid p = sid <> upperFirst (camel p)

-- | @enumCtor "CandidateFinishReason" "FINISH_REASON_STOP"@ → @CandidateFinishReasonFinishReasonStop@.
enumCtor :: Text -> Text -> Text
enumCtor ety v = ety <> guardDigit pascal
  where
    pascal = T.concat [upperFirst (T.toLower w) | w <- T.splitOn "_" v, not (T.null w)]
    guardDigit x = if maybe False (isDigit . fst) (T.uncons x) then "X" <> x else x

-- | What a scalar/element type is, for presence decisions.
data Kind
  = KMessage
  | -- | default expression
    KScalar Text
  | -- | no sensible default: UTCTime, Value, enum without UNSPECIFIED
    KNoDefault
  | -- | Object / Map
    KMono

-- | Types a non-array property or an array element / map value.
elemOf :: Text -> Text -> Property -> Either String (Text, Wire, Kind, [EnumDef])
elemOf sid pname p
  | Just r <- propRef p = Right (r, WPlain, KMessage, [])
  | otherwise = case fromMaybe "" (propType p) of
      "string"
        | not (null (propEnum p)) ->
            let ety = enumTypeName sid pname
                ctors = [(enumCtor ety v, v, d) | (v, d) <- zip (propEnum p) (propEnumDescriptions p ++ repeat "")]
                kind = case [c | (c, v, _) <- ctors, "UNSPECIFIED" `T.isSuffixOf` v] of
                  (c : _) -> KScalar c
                  [] -> KNoDefault
             in Right (ety, WPlain, kind, [EnumDef ety (propDescription p) ctors])
        | propFormat p `elem` [Just "int64", Just "uint64"] -> Right ("Int64", WInt64, KScalar "0", [])
        | propFormat p == Just "byte" -> Right ("Base64Bytes", WPlain, KScalar "(Base64Bytes mempty)", [])
        | propFormat p `elem` [Just "google-datetime", Just "date-time"] -> Right ("UTCTime", WPlain, KNoDefault, [])
        | otherwise -> Right ("Text", WPlain, KScalar "\"\"", [])
      "integer" -> Right ("Int", WPlain, KScalar "0", [])
      "number" -> Right ("Double", WPlain, KScalar "0", [])
      "boolean" -> Right ("Bool", WPlain, KScalar "False", [])
      "object" -> case propAdditional p of
        Just ap | isAny ap -> Right ("Object", WPlain, KMono, [])
        Just ap -> do
          (vt, w, _, es) <- elemOf sid pname ap
          if w == WInt64
            then Left (where_ <> ": int64 map values are unsupported")
            else Right ("Map Text " <> vt, WPlain, KMono, es)
        Nothing -> Right ("Object", WPlain, KMono, [])
      "any" -> Right ("Value", WPlain, KNoDefault, [])
      "array" -> Left (where_ <> ": nested arrays are unsupported")
      other -> Left (where_ <> ": unsupported type " <> T.unpack other)
  where
    where_ = T.unpack sid <> "." <> T.unpack pname
    isAny ap = propType ap == Just "any" || (propType ap == Nothing && propRef ap == Nothing)

-- | Applies the spec's typing table to one property.
fieldOf :: Direction -> Text -> Text -> Property -> Either String (Field, [EnumDef])
fieldOf dir sid pname p = do
  (ty, wire, kind, enums) <- case propType p of
    Just "array" -> case propItems p of
      Nothing -> Left (T.unpack sid <> "." <> T.unpack pname <> ": array without items")
      Just it -> do
        (et, w, _, es) <- elemOf sid pname it
        Right ("[" <> et <> "]", w, KMono, es)
    _ -> elemOf sid pname p
  let req = isRequired p
      (fty, pres) = case kind of
        KMono -> (ty, Mono)
        _ | req -> (ty, Required)
        KMessage -> ("Maybe " <> ty, Optional)
        KNoDefault -> ("Maybe " <> ty, Optional)
        KScalar d
          | dir == ResponseOnly -> (ty, Defaulted d)
          | otherwise -> ("Maybe " <> ty, Optional)
      (fty', pres') = case Map.lookup (sid, pname) overrides of
        Nothing -> (fty, pres)
        Just ForceMaybe -> ("Maybe " <> ty, Optional)
        Just ForceRequired -> (ty, Required)
        Just (ForceDefault d) -> (ty, Defaulted d)
  pure
    ( Field
        { fJson = pname
        , fName = hsFieldName sid pname
        , fType = fty'
        , fBase = ty
        , fWire = wire
        , fPresence = pres'
        , fPositional = req
        , fDoc = (if propDeprecated p then "Deprecated. " else "") <> propDescription p
        }
    , enums
    )

typeDefOf :: Map Text Direction -> Schema -> Either String TypeDef
typeDefOf dirs s = do
  let dir = fromMaybe Bidirectional (Map.lookup (schemaId s) dirs)
  fs <- traverse (\(n, p) -> fieldOf dir (schemaId s) n p) (Map.toAscList (schemaProperties s))
  pure
    TypeDef
      { tName = schemaId s
      , tDoc = schemaDescription s
      , tFields = map fst fs
      , tEnums = concatMap snd fs
      }
