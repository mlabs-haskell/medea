{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Spec.Schema where 

import Control.Applicative (Alternative)
import Control.Applicative.Permutations (Permutation,
                                         runPermutation,
                                         toPermutation,
                                         toPermutationWithDefault)
import Data.Text (Text)
import Text.Megaparsec (MonadParsec(..), (<|>))
import Text.Megaparsec.Char (eol, char)

import Data.Medea.Parser.Types (MedeaParser, ParseError)
import Data.Medea.Parser.Primitive (Identifier, parseKeyVal,
                                    parseIdentifier, parseLine)

import qualified Data.Vector as V
import qualified Data.Medea.Parser.Spec.Array as Array
import qualified Data.Medea.Parser.Spec.Object as Object
import qualified Data.Medea.Parser.Spec.Type as Type

data Specification = Specification {
  name :: !Identifier,
  types :: !Type.Specification,
  array :: !Array.Specification,
  object :: !Object.Specification
}
  deriving (Eq)

parseSpecification :: MedeaParser Specification
parseSpecification = do
  schemaName <- parseLine 0 $ parseKeyVal "schema" parseIdentifier
  runPermutation $ Specification schemaName
    <$> toPermutationWithDefault Type.defaultSpec (try Type.parseSpecification)
    <*> toPermutationWithDefault Array.defaultSpec (try Array.parseSpecification)
    <*> toPermutationWithDefault Object.defaultSpec (try Object.parseSpecification)
