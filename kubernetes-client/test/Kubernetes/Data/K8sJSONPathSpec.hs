{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.Data.K8sJSONPathSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as P

import Kubernetes.Data.K8sJSONPath
import Data.Text
import Data.JSONPath
import Data.Aeson
import Data.Void 

spec :: Spec
spec = do
  describe "K8sJSONPath" $ do
    describe "Parsing" $ do
      it "should parse plain text" $ do
        ("plain" :: Text) ~> k8sJSONPath
          `shouldParse` [PlainText "plain"]

      it "should parse jsonpath" $ do
        ("{.foo}" :: Text) ~> k8sJSONPath
          `shouldParse` [JSONPath [KeyChild "foo"]]

      it "should parse K8sJSONPath with both text and jsonpath" $ do
        ("kind is {.kind}" :: Text) ~> k8sJSONPath
          `shouldParse` [PlainText "kind is ", JSONPath [KeyChild "kind"]]

    describe "Running" $ do
      it "should interpolate string with json values" $ do
        let path = [PlainText "kind is ", JSONPath [KeyChild "kind"]]
            val = (object ["kind" .= ("Pod" :: Text)])
        runJSONPath path val `shouldBe` Right "kind is Pod"

(~>) :: Text -> Parser [K8sPathElement] -> Either (P.ParseErrorBundle Text Void) [K8sPathElement]
s ~> p = P.parse p "" s
