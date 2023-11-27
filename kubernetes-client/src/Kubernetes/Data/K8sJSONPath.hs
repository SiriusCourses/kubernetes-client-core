{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Kubernetes.Data.K8sJSONPath where

import Data.Aeson
import Data.Aeson.Text
import Data.JSONPath
import Data.Text       as Text

import Control.Applicative  ((<|>))
import Data.Text.Lazy       (toStrict)
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P


data K8sPathElement = PlainText Text
                    | JSONPath [JSONPathElement]
  deriving  (Show, Eq)

type Parser = P.Parsec Void Text 

k8sJSONPath :: Parser [K8sPathElement]
k8sJSONPath = P.some pathElementParser

pathElementParser :: Parser K8sPathElement
pathElementParser = jsonpathParser <|> plainTextParser

plainTextParser :: Parser K8sPathElement
plainTextParser = PlainText <$> P.takeWhile1P Nothing (/= '{')

jsonpathParser :: Parser K8sPathElement
jsonpathParser = JSONPath <$> (P.char '{' *> jsonPath (P.char '}') <* P.char '}')

runJSONPath :: [K8sPathElement] -> Value -> Either String Text
runJSONPath [] _ = pure ""
runJSONPath (e:es) v = do
  res <- runPathElement e v
  rest <- runJSONPath es v
  pure $ res <> rest

runPathElement :: K8sPathElement -> Value -> Either String Text
runPathElement (PlainText t) _ = pure t
runPathElement (JSONPath []) _ = Left "empty json path"
runPathElement (JSONPath p) v = Right $ encodeResult $ executeJSONPath p v

encodeResult :: [Value] -> Text
encodeResult = \case
  [val] -> jsonToText val
  vals -> (intercalate " " $ Prelude.map jsonToText vals)

jsonToText :: Value -> Text
jsonToText (String t) = t
jsonToText x          = toStrict $ encodeToLazyText x