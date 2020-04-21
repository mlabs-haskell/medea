module Main where

import Data.Foldable (traverse_)
import Data.Medea (loadSchemaFromFile)
import Test.Hspec (Spec, describe, hspec, it, runIO, shouldSatisfy, shouldNotSatisfy)
import TestM.Util (listMedeaFiles)
import TestM (isParseError, runTestM)

main :: IO ()
main = do
  let failDir = "./conformance/parser/fail"
      passDir = "./conformance/parser/pass"
  failTestFiles <- listMedeaFiles failDir
  passTestFiles <- listMedeaFiles passDir
  hspec . describe "Invalid parse cases" . traverse_ makeParseTestFail $ failTestFiles
  hspec . describe "Valid parse cases" . traverse_ makeParseTestPass $ passTestFiles

-- Helpers

makeParseTestFail :: FilePath -> Spec
makeParseTestFail fp = do
  result <- runIO . runTestM . loadSchemaFromFile $ fp
  it ("Shouldn't parse: " ++ fp) (result `shouldSatisfy` isParseError)

makeParseTestPass :: FilePath -> Spec
makeParseTestPass fp = do
  result <- runIO . runTestM . loadSchemaFromFile $ fp
  it ("Should parse: " ++ fp) 
    (do
      result `shouldNotSatisfy` isParseError
    )
