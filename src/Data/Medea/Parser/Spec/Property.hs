{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Spec.Property
  ( Specification (..),
    parseSpecification,
  )
where

import Data.Functor (($>))
import Data.Medea.Parser.Primitive
  ( Identifier,
    MedeaString,
    parseIdentifier,
    parseKeyVal,
    parseLine,
    parseReservedChunk,
    parseString,
  )
import Data.Medea.Parser.Types (MedeaParser)
import Text.Megaparsec (MonadParsec (..), option, try)

data Specification
  = Specification
      { propName :: MedeaString,
        propSchema :: Maybe Identifier,
        propOptional :: Bool
      }
  deriving (Eq)

parseSpecification :: MedeaParser Specification
parseSpecification =
  Specification
    <$> parsePropName
    <*> parsePropSchema
    <*> parsePropOptional
  where
    parsePropName =
      parseLine 8 $
        parseKeyVal "property-name" parseString
    parsePropSchema =
      option Nothing . try . parseLine 8 $
        Just <$> parseKeyVal "property-schema" parseIdentifier
    parsePropOptional =
      option False . try . parseLine 8 $
        parseReservedChunk "optional-property" $> True
