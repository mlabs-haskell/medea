module Main where

import Data.Either (isLeft)
import Data.List (sort)
import Test.Hspec (Spec, hspec, describe, runIO, it, shouldSatisfy)
import System.FilePath (isExtensionOf, (</>))
import Data.Foldable (traverse_)
import System.Directory (listDirectory)

import Data.Medea.Loader (loadSchemaFromFile)

main :: IO ()
main = do
  let prefix = "./conformance/parser"
  testFiles <- fmap (prefix </>) . sort . filter (isExtensionOf ".medea") <$> listDirectory prefix
  hspec . describe "Invalid parse cases" . traverse_ makeParseTest $ testFiles

-- Helpers

makeParseTest :: FilePath -> Spec
makeParseTest fp = do
  result <- runIO . loadSchemaFromFile $ fp
  it ("Shouldn't parse: " ++ fp) (result `shouldSatisfy` isLeft) 