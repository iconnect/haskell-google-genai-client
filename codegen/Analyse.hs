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
  , Seg (..)
  , ResourceTpl (..)
  , QueryParam (..)
  , Endpoint (..)
  , Plan (..)
  , endpointOf
  , analyse
  ) where

import Data.Char (isDigit, toLower, toUpper)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Discovery hiding (Param)

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
camel t = leading <> camelRest rest
  where
    (leading, rest) = T.span (\c -> c == '_' || c == '-') t
    camelRest t' = case filter (not . T.null) (T.split (\c -> c == '_' || c == '-') t') of
      [] -> t'
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
                kind = case [c | (c, v, _) <- ctors, "UNSPECIFIED" `T.isSuffixOf` T.toUpper v] of
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

-- | Emitted field names are namespaced by the schema id (see 'hsFieldName'),
-- so a name can only collide with a sibling field of the *same* schema --
-- cross-schema collisions can't happen here and are left to Task 6, which
-- sees the whole plan.
typeDefOf :: Map Text Direction -> Schema -> Either String TypeDef
typeDefOf dirs s = do
  let dir = fromMaybe Bidirectional (Map.lookup (schemaId s) dirs)
  fs <- traverse (\(n, p) -> fieldOf dir (schemaId s) n p) (Map.toAscList (schemaProperties s))
  let flds = map fst fs
      byName = Map.fromListWith (++) [(fName f, [fJson f]) | f <- flds]
      dups = [(n, js) | (n, js) <- Map.toAscList byName, length js > 1]
  case dups of
    (n, js) : _ ->
      Left
        ( T.unpack (schemaId s) <> ": field name " <> T.unpack n
            <> " collides across JSON keys " <> T.unpack (T.intercalate ", " js)
        )
    [] ->
      pure
        TypeDef
          { tName = schemaId s
          , tDoc = schemaDescription s
          , tFields = flds
          , tEnums = concatMap snd fs
          }

data Seg = Lit Text | Param Text
  deriving (Show, Eq)

data ResourceTpl
  = -- | @v1beta/{+model}<verb>@; verb includes the leading colon
    TplModel Text
  | -- | any other path, minus the @v1beta/@ prefix
    TplRaw [Seg]
  deriving (Show, Eq)

data QueryParam = QueryParam
  { qpJson :: Text
  , qpField :: Text
  , qpType :: Text
  -- ^ @Int@, @Bool@ or @Text@
  }
  deriving (Show, Eq)

data Endpoint = Endpoint
  { epName :: Text
  , epHttp :: Text
  , epPath :: Text
  -- ^ original Discovery path, for docs
  , epPathParams :: [Text]
  -- ^ in parameterOrder
  , epResource :: ResourceTpl
  , epQuery :: [QueryParam]
  , epBody :: Maybe Text
  , epResponse :: Maybe Text
  -- ^ Nothing = @Empty@ → @Request ()@
  , epAlt :: Maybe Text
  , epDoc :: Text
  }
  deriving (Show, Eq)

data Plan = Plan
  { planRevision :: Text
  , planTypes :: [TypeDef]
  , planEndpoints :: [Endpoint]
  , planDirections :: Map Text Direction
  }
  deriving (Show)

-- | Bare-verb methods get resource-qualified names. Anything else keeps the
-- Discovery method name.
fnNames :: Map Text Text
fnNames =
  Map.fromList
    [ ("generativelanguage.models.list", "listModels")
    , ("generativelanguage.models.get", "getModel")
    , ("generativelanguage.files.get", "getFile")
    , ("generativelanguage.files.list", "listFiles")
    , ("generativelanguage.files.delete", "deleteFile")
    , ("generativelanguage.cachedContents.create", "createCachedContent")
    , ("generativelanguage.cachedContents.get", "getCachedContent")
    , ("generativelanguage.cachedContents.list", "listCachedContents")
    , ("generativelanguage.cachedContents.patch", "patchCachedContent")
    , ("generativelanguage.cachedContents.delete", "deleteCachedContent")
    ]

-- | @"files/{+name}:download"@ → @[Lit "files/", Param "name", Lit ":download"]@.
parseTpl :: Text -> [Seg]
parseTpl t
  | T.null t = []
  | otherwise = case T.breakOn "{" t of
      (lit, rest)
        | T.null rest -> [Lit lit]
        | otherwise ->
            let (inner, rest') = T.breakOn "}" (T.drop 1 rest)
                name = fromMaybe inner (T.stripPrefix "+" inner)
             in [Lit lit | not (T.null lit)] ++ [Param name] ++ parseTpl (T.drop 1 rest')

endpointOf :: Method -> Either String Endpoint
endpointOf m = do
  rest <- maybe (Left ("path without v1beta/ prefix: " <> T.unpack (methodPath m))) Right (T.stripPrefix "v1beta/" (methodPath m))
  let short = T.takeWhileEnd (/= '.') (methodId m)
      name = fromMaybe short (Map.lookup (methodId m) fnNames)
      resource = maybe (TplRaw (parseTpl rest)) TplModel (T.stripPrefix "{+model}" rest)
      pathParams = [n | n <- methodParamOrder m, Just pr <- [Map.lookup n (methodParams m)], paramLocation pr == "path"]
      query = [QueryParam n (name <> upperFirst (camel n)) (paramHs pr) | (n, pr) <- Map.toAscList (methodParams m), paramLocation pr == "query"]
  pure
    Endpoint
      { epName = name
      , epHttp = methodHttp m
      , epPath = methodPath m
      , epPathParams = pathParams
      , epResource = resource
      , epQuery = query
      , epBody = methodRequest m
      , epResponse = case methodResponse m of
          Just "Empty" -> Nothing
          r -> r
      , epAlt = if short == "streamGenerateContent" then Just "sse" else Nothing
      , epDoc = methodDescription m
      }
  where
    paramHs pr = case paramType pr of
      "integer" -> "Int"
      "boolean" -> "Bool"
      _ -> "Text"

-- | @analyse allowlist typesOnly doc@. Endpoints are emitted for the allowlist
-- only; schemas for the closure of both lists.
--
-- Beyond the schema/enum type-name check, every generated name lands in one
-- Haskell module, so field names and enum constructor names share that
-- namespace too (e.g. schema @Foo@ with property @barBaz@ and schema
-- @FooBar@ with property @baz@ both yield the field name @fooBarBaz@). The
-- dup check below is namespace-wide: schema type names, enum type names,
-- record field names, and enum constructor names (including the
-- @<Enum>Unknown@ fallback constructor emitted per-enum by Task 7).
analyse :: [Text] -> [Text] -> Doc -> Either String Plan
analyse allow typesOnly doc = do
  allowed <- traverse lookupMethod allow
  extra <- traverse lookupMethod typesOnly
  let dirs = directions doc (allowed ++ extra)
  types <- traverse (lookupType dirs) (Map.keys dirs)
  eps <- traverse endpointOf (sortOn methodId allowed)
  let names =
        map tName types
          ++ concatMap (map enumName . tEnums) types
          ++ concatMap (map fName . tFields) types
          ++ concatMap enumCtorNames types
      dups = Map.keys (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(n, 1) | n <- names]))
  if null dups
    then pure Plan {planRevision = docRevision doc, planTypes = types, planEndpoints = eps, planDirections = dirs}
    else Left ("generated name collisions: " <> show dups)
  where
    lookupMethod ident =
      maybe (Left ("unknown method id: " <> T.unpack ident)) Right (Map.lookup ident (docMethods doc))
    lookupType dirs sid =
      maybe (Left ("unknown schema: " <> T.unpack sid)) (typeDefOf dirs) (Map.lookup sid (docSchemas doc))
    enumCtorNames td =
      concat
        [ (enumName e <> "Unknown") : [c | (c, _, _) <- enumCtors e]
        | e <- tEnums td
        ]
