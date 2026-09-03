{-# LANGUAGE OverloadedStrings #-}

-- | The subset of Google's Discovery document format the codegen reads.
module Discovery
  ( Doc (..)
  , Schema (..)
  , Property (..)
  , Method (..)
  , Param (..)
  , parseDoc
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

data Doc = Doc
  { docRevision :: Text
  , docSchemas :: Map Text Schema
  , docMethods :: Map Text Method
  -- ^ keyed by method id, flattened over nested resources
  }
  deriving (Show)

data Schema = Schema
  { schemaId :: Text
  , schemaDescription :: Text
  , schemaProperties :: Map Text Property
  }
  deriving (Show)

data Property = Property
  { propType :: Maybe Text
  , propFormat :: Maybe Text
  , propRef :: Maybe Text
  , propItems :: Maybe Property
  , propEnum :: [Text]
  , propEnumDescriptions :: [Text]
  , propAdditional :: Maybe Property
  , propDescription :: Text
  , propDeprecated :: Bool
  }
  deriving (Show, Eq)

data Method = Method
  { methodId :: Text
  , methodPath :: Text
  , methodHttp :: Text
  , methodParams :: Map Text Param
  , methodParamOrder :: [Text]
  , methodRequest :: Maybe Text
  , methodResponse :: Maybe Text
  , methodDescription :: Text
  }
  deriving (Show)

data Param = Param
  { paramType :: Text
  , paramFormat :: Maybe Text
  , paramLocation :: Text
  , paramRequired :: Bool
  , paramDescription :: Text
  , paramPattern :: Maybe Text
  }
  deriving (Show)

instance FromJSON Property where
  parseJSON = withObject "Property" $ \o ->
    Property
      <$> o .:? "type"
      <*> o .:? "format"
      <*> o .:? "$ref"
      <*> o .:? "items"
      <*> o .:? "enum" .!= []
      <*> o .:? "enumDescriptions" .!= []
      <*> o .:? "additionalProperties"
      <*> o .:? "description" .!= ""
      <*> o .:? "deprecated" .!= False

instance FromJSON Schema where
  parseJSON = withObject "Schema" $ \o ->
    Schema
      <$> o .: "id"
      <*> o .:? "description" .!= ""
      <*> o .:? "properties" .!= Map.empty

instance FromJSON Param where
  parseJSON = withObject "Param" $ \o ->
    Param
      <$> o .:? "type" .!= "string"
      <*> o .:? "format"
      <*> o .:? "location" .!= "query"
      <*> o .:? "required" .!= False
      <*> o .:? "description" .!= ""
      <*> o .:? "pattern"

instance FromJSON Method where
  parseJSON = withObject "Method" $ \o ->
    Method
      <$> o .: "id"
      <*> o .: "path"
      <*> o .: "httpMethod"
      <*> o .:? "parameters" .!= Map.empty
      <*> o .:? "parameterOrder" .!= []
      <*> refOf o "request"
      <*> refOf o "response"
      <*> o .:? "description" .!= ""

refOf :: Object -> Key -> Parser (Maybe Text)
refOf o k = do
  sub <- o .:? k
  case sub of
    Nothing -> pure Nothing
    Just (s :: Object) -> s .:? "$ref"

instance FromJSON Doc where
  parseJSON = withObject "Doc" $ \o -> do
    rev <- o .: "revision"
    schemas <- o .:? "schemas" .!= Map.empty
    resources <- o .:? "resources" .!= (Map.empty :: Map Text Value)
    methods <- concat <$> traverse resourceMethods (Map.elems resources)
    pure (Doc rev schemas (Map.fromList [(methodId m, m) | m <- methods]))

resourceMethods :: Value -> Parser [Method]
resourceMethods = withObject "resource" $ \o -> do
  ms <- o .:? "methods" .!= (Map.empty :: Map Text Method)
  subs <- o .:? "resources" .!= (Map.empty :: Map Text Value)
  rest <- concat <$> traverse resourceMethods (Map.elems subs)
  pure (Map.elems ms ++ rest)

parseDoc :: LBS.ByteString -> Either String Doc
parseDoc = eitherDecode
