{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module: Data.Medea
-- Description: A JSON schema language validator.
-- Copyright: (C) Juspay Technologies Pvt Ltd, 2020-21
-- License: MIT
-- Maintainer: koz.ross@retro-freedom.nz
-- Stability: Experimental
-- Portability: GHC only
--
-- This module contains the reference Haskell implementation of a Medea
-- validator, providing both schema graph file loading and validation, with some
-- convenience functions.
--
-- A minimal example of use follows. This example first attempts to load a Medea
-- schema graph file from @\/path\/to\/schema.medea@, and, if successful, attempts
-- to validate the JSON file at @\/path\/to\/my.json@ against the schemata so
-- loaded.
--
-- > import Data.Medea (loadSchemaFromFile, validateFromFile)
-- >
-- > main :: IO ()
-- > main = do
-- >   -- try to load the schema graph file
-- >   loaded <- loadSchemaFromFile "/path/to/schema.medea"
-- >   case loaded of
-- >      Left err -> print err -- or some other handling
-- >      Right scm -> do
-- >        -- try to validate
-- >        validated <- validateFromFile scm "/path/to/my.json"
-- >        case validated of
-- >          Left err -> print err -- or some other handling
-- >          Right validJson -> print validJson -- or some other useful thing
--
-- For more details about how to create Medea schema graph files, see
-- @TUTORIAL.md@ and @SPEC.md@.
module Data.Medea
  ( -- * Schema loading
    Schema,
    LoaderError (..),
    ParseError (..),
    buildSchema,
    loadSchemaFromFile,
    loadSchemaFromHandle,

    -- * Schema validation
    JSONType (..),
    SchemaInformation (..),
    ValidationError (..),
    ValidatedJSON (..),
    validate,
    validateFromFile,
    validateFromHandle,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
  ( Value,
    eitherDecodeFileStrict,
    eitherDecodeStrict,
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Hashable (Hashable (..))
import Data.Medea.Analysis
  ( TypeNode (AnyNode, CustomNode),
  )
import Data.Medea.JSONType (JSONType (..))
import Data.Medea.Loader
  ( LoaderError (..),
    buildSchema,
    loadSchemaFromFile,
    loadSchemaFromHandle,
  )
import Data.Medea.Parser.Primitive
  ( ReservedIdentifier (..),
    identFromReserved,
  )
import Data.Medea.Parser.Types (ParseError (..))
import Data.Medea.Schema (Schema (..))
import Data.Medea.ValidJSON (ValidJSONF (..))
import Data.Text (Text, pack)
import Data.Vector (Vector)
import System.IO (Handle)

-- | An annotation, describing which schema a given chunk of JSON was deemed to
-- be valid against.
--
-- @since 1.3.0
data SchemaInformation
  = -- | No requirements were placed on this chunk.
    -- @since 1.3.0
    AnySchema
  | -- | Validated as JSON @null@.
    -- @since 1.3.0
    NullSchema
  | -- | Validated as JSON boolean.
    -- @since 1.3.0
    BooleanSchema
  | -- | Validated as JSON number.
    -- @since 1.3.0
    NumberSchema
  | -- | Validated as JSON string.
    -- @since 1.3.0
    StringSchema
  | -- | Validated as JSON array.
    -- @since 1.3.0
    ArraySchema
  | -- | Validated as JSON object.
    -- @since 1.3.0
    ObjectSchema
  | -- | Validated against the start schema.
    -- @since 1.3.0
    StartSchema
  | -- | Validated against the schema with the given name.
    -- @since 1.3.0
    UserDefined {-# UNPACK #-} !Text
  deriving stock
    ( -- | @since 1.3.0
      Eq,
      -- | @since 1.3.0
      Show
    )

-- | @since 1.3.0
instance Hashable SchemaInformation where
  {-# INLINEABLE hashWithSalt #-}
  hashWithSalt salt = \case
    AnySchema -> salt `hashWithSalt` (0 :: Int)
    NullSchema -> salt `hashWithSalt` (1 :: Int)
    BooleanSchema -> salt `hashWithSalt` (2 :: Int)
    NumberSchema -> salt `hashWithSalt` (3 :: Int)
    StringSchema -> salt `hashWithSalt` (4 :: Int)
    ArraySchema -> salt `hashWithSalt` (5 :: Int)
    ObjectSchema -> salt `hashWithSalt` (6 :: Int)
    StartSchema -> salt `hashWithSalt` (7 :: Int)
    UserDefined t -> salt `hashWithSalt` t `hashWithSalt` (8 :: Int)

-- | @since 1.3.0
instance NFData SchemaInformation where
  {-# INLINEABLE rnf #-}
  rnf = \case
    UserDefined t -> seq t ()
    si -> seq si ()

-- | The result of validation of JSON against a Medea schema.
--
-- @since 1.3.0
data ValidatedJSON
  = -- | This JSON was deemed invalid, after trying the given schemata. We
    -- give the exact error for each schema.
    --
    -- @since 1.3.0
    Invalid (Vector (SchemaInformation, ValidationError))
  | -- | This JSON validated against the given schema, and has the given type.
    -- @since 1.3.0
    Valid SchemaInformation (ValidJSONF ValidatedJSON)
  deriving stock
    ( -- | @since 1.3.0
      Eq,
      -- | @since 1.3.0
      Show
    )

-- | @since 1.3.0
instance NFData ValidatedJSON where
  {-# INLINE rnf #-}
  rnf = \case
    Invalid errs -> errs `seq` ()
    Valid si jsonf -> si `seq` jsonf `seq` ()

-- | @since 1.3.0
instance Hashable ValidatedJSON where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt = \case
    Invalid errs -> salt `hashWithSalt` errs `hashWithSalt` (0 :: Int)
    Valid si jsonf ->
      salt `hashWithSalt` si `hashWithSalt` jsonf `hashWithSalt` (1 :: Int)

-- | All possible validation errors.
--
-- @since 1.3.0
data ValidationError
  = -- | We got a type different to what we expected.
    -- @since 1.3.0
    WrongType
      !Value
      -- ^ The chunk of JSON.
      -- @since 1.3.0
      !JSONType
      -- ^ What we expected the type to be.
      -- @since 1.3.0
  | -- | We found a JSON object with a property that wasn't specified in its
    -- schema, and additional properties are forbidden. Gives the name of the
    -- property that caused the error.
    --
    -- @since 1.3.0
    AdditionalPropFoundButBanned {-# UNPACK #-} !Text
  | -- | We found a JSON object which is missing a property its schema
    -- requires. Gives the name of the property that caused the error.
    --
    -- @since 1.3.0
    RequiredPropertyIsMissing {-# UNPACK #-} !Text
  | -- | We found a JSON array which falls outside of the minimum or maximum
    -- length constraints its corresponding schema demands. Also gives the JSON
    -- chunk corresponding to the invalid array.
    --
    -- @since 1.3.0
    OutOfBoundsArrayLength !Value
  | -- | This is a bug - please report it to us!
    -- @since 1.3.0
    ImplementationError
      {-# UNPACK #-} !Text -- some descriptive text
  deriving stock
    ( -- | @since 1.3.0
      Eq,
      -- | @since 1.3.0
      Show
    )

-- | @since 1.3.0
instance Hashable ValidationError where
  {-# INLINEABLE hashWithSalt #-}
  hashWithSalt salt = \case
    WrongType v t ->
      salt `hashWithSalt` v `hashWithSalt` t `hashWithSalt` (0 :: Int)
    AdditionalPropFoundButBanned prop ->
      salt `hashWithSalt` prop `hashWithSalt` (1 :: Int)
    RequiredPropertyIsMissing prop ->
      salt `hashWithSalt` prop `hashWithSalt` (2 :: Int)
    OutOfBoundsArrayLength v ->
      salt `hashWithSalt` v `hashWithSalt` (3 :: Int)
    ImplementationError err ->
      salt `hashWithSalt` err `hashWithSalt` (4 :: Int)

-- | Attempt to construct validated JSON from a strict bytestring.
-- This will attempt to decode using Aeson before validating, and will return a
-- 'Left' containing its textual error message on failure.
--
-- @since 1.3.0
validate :: Schema -> ByteString -> Either Text ValidatedJSON
validate scm bs = case eitherDecodeStrict bs of
  Left err -> Left . pack $ err
  Right val -> Right . validate' val $ scm

-- | Helper for construction of validated JSON from a JSON file.
-- This will attempt to decode using Aeson before validating.
--
-- This will clean up any file handle(s) if any exceptions are thrown.
--
-- @since 1.3.0
validateFromFile ::
  (MonadIO m) =>
  Schema ->
  FilePath ->
  m (Either Text ValidatedJSON)
validateFromFile scm fp = go <$> (liftIO . eitherDecodeFileStrict $ fp)
  where
    go :: Either String Value -> Either Text ValidatedJSON
    go = \case
      Left err -> Left . pack $ err
      Right val ->
        Right . validate' val $ scm

-- | Helper for construction of validated JSON from a 'Handle'. This will
-- attempt to decode using Aeson before validating.
--
-- This will close the 'Handle' upon finding EOF, or if an exception is thrown.
--
-- @since 1.3.0
validateFromHandle ::
  (MonadIO m) =>
  Schema ->
  Handle ->
  m (Either Text ValidatedJSON)
validateFromHandle scm h = go . eitherDecodeStrict <$> (liftIO . BS.hGetContents $ h)
  where
    go :: Either String Value -> Either Text ValidatedJSON
    go = \case
      Left err -> Left . pack $ err
      Right val ->
        Right . validate' val $ scm

-- Helpers

validate' :: Value -> Schema -> ValidatedJSON
validate' val scm = _

{-
newtype ValidationM a = ValidationM
  { runValidationM ::
      RWST
        Schema
        ()
        (NESet TypeNode, Maybe Identifier)
        (Either ValidationError)
        a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader Schema,
      MonadState (NESet TypeNode, Maybe Identifier),
      MonadError ValidationError
    )

instance Alternative ValidationM where
  empty = ValidationM . RWST $ \_ _ -> Left EmptyError
  ValidationM comp1 <|> ValidationM comp2 = ValidationM . RWST $ go
    where
      go r s = case runRWST comp1 r s of
        Left err -> case runRWST comp2 r s of
          Left _ -> Left err
          Right res -> Right res
        Right res -> Right res

failWith :: ValidationError -> ValidationM a
failWith err = ValidationM . RWST $ \_ _ -> Left err

-- We have 3 different cases:
-- 1. If we are checking against AnyNode, we ALWAYS succeed.
-- 2. If we are checking against PrimitiveNode, we can match with EXACTLY ONE
--    kind of PrimitiveNode.
-- 3. If we are checking against CustomNode, we can match against ANY CustomNode.
--    Thus, we must try all of them.
checkTypes :: Value -> ValidationM (Cofree ValidJSONF SchemaInformation)
checkTypes v = checkAny v <|> checkPrim v <|> checkCustoms v

-- checkAny throws EmptyError if AnyNode is not found. This lets checkTypes
-- use the error thrown by checkPrim/checkCustoms if checkAny fails.
checkAny :: Value -> ValidationM (Cofree ValidJSONF SchemaInformation)
checkAny v = do
  minNode <- gets (findMin . fst)
  case minNode of
    AnyNode -> pure (AnySchema :< AnythingF v)
    _ -> failWith EmptyError

-- checkPrim searches the NESet for the PrimitiveNode corresponding to the Value, otherwise throws an error.
checkPrim :: Value -> ValidationM (Cofree ValidJSONF SchemaInformation)
checkPrim v = do
  (nodes, par) <- get
  unless (member (PrimitiveNode . typeOf $ v) nodes) (failWith . NotOneOfOptions $ v)
  case v of
    Null -> pure (NullSchema :< NullF)
    Bool b -> pure (BooleanSchema :< BooleanF b)
    Number n -> pure (NumberSchema :< NumberF n)
    String s -> case par of
      -- if we are checking a dependent string, we match against the supplied
      -- values
      Nothing -> pure (StringSchema :< StringF s)
      Just parIdent -> do
        scm <- lookupSchema parIdent
        let validVals = stringVals scm
        if
            | V.length validVals == 0 -> pure (StringSchema :< StringF s)
            | s `V.elem` validVals -> pure (StringSchema :< StringF s)
            | otherwise -> failWith . NotOneOfOptions $ v
    Array arr -> case par of
      Nothing -> put (anySet, Nothing) >> (ArraySchema :<) . ArrayF <$> traverse checkTypes arr
      Just parIdent -> checkArray arr parIdent
    Object obj -> case par of
      -- Fast path (no object spec)
      Nothing ->
        put (anySet, Nothing) >> (ObjectSchema :<) . ObjectF <$> traverse checkTypes obj
      Just parIdent -> checkObject obj parIdent

-- check if the array satisfies the corresponding specification.
checkArray :: Array -> Identifier -> ValidationM (Cofree ValidJSONF SchemaInformation)
checkArray arr parIdent = do
  scm <- lookupSchema parIdent
  let arrLen = fromIntegral . V.length $ arr
  maybe (failWith outOfBounds) pure $ case arrayBounds scm of
    Non -> Just () -- no bounds, so any array will do
    One lo -> unless (arrLen >= lo) Nothing
    Eno hi -> unless (arrLen <= hi) Nothing
    Two lo hi -> unless (arrLen >= lo && arrLen <= hi) Nothing
  let valsAndTypes = pairValsWithTypes . arrayTypes $ scm
  checkedArray <- traverse go valsAndTypes
  pure (ArraySchema :< ArrayF checkedArray)
  where
    outOfBounds = OutOfBoundsArrayLength (textify parIdent) . Array $ arr
    pairValsWithTypes = \case
      Nothing -> (,AnyNode) <$> arr
      Just (ListType node) -> (,node) <$> arr
      Just (TupleType nodes) -> V.zip arr nodes
    go (val, typeNode) = do
      put (singleton typeNode, Nothing)
      checkTypes val

-- check if object properties satisfy the corresponding specification.
checkObject :: Object -> Identifier -> ValidationM (Cofree ValidJSONF SchemaInformation)
checkObject obj parIdent = do
  valsAndTypes <- pairPropertySchemaAndVal obj parIdent
  checkedObj <- traverse go valsAndTypes
  pure (ObjectSchema :< ObjectF checkedObj)
  where
    go (val, typeNode) = do
      put (singleton typeNode, Nothing)
      checkTypes val

pairPropertySchemaAndVal ::
  HashMap Text Value -> Identifier -> ValidationM (HashMap Text (Value, TypeNode))
pairPropertySchemaAndVal obj parIdent = do
  scm <- lookupSchema parIdent
  mappedObj <- traverse (pairProperty scm) . HM.mapWithKey (,) $ obj
  traverse_ isMatched . HM.mapWithKey (,) . props $ scm
  pure mappedObj
  where
    -- maps each property value with the schema it should validate against
    pairProperty scm (propName, v) = case HM.lookup propName . props $ scm of
      Just (typeNode, _) -> pure (v, typeNode)
      Nothing ->
        if additionalProps scm
          then pure (v, additionalPropSchema scm)
          else failWith . AdditionalPropFoundButBanned (textify parIdent) $ propName
    -- throws an error if a non-optional property was not found in the object
    isMatched (propName, (_, optional)) = case HM.lookup propName obj of
      Nothing ->
        unless optional . failWith . RequiredPropertyIsMissing (textify parIdent) $ propName
      Just _ -> pure ()

-- checkCustoms removes all non custom nodes from the typeNode set and
-- checks the Value against each until one succeeds.
checkCustoms :: Value -> ValidationM (Cofree ValidJSONF SchemaInformation)
checkCustoms v = do
  -- Here we drop all non-custom nodes
  customNodes <- gets (dropWhileAntitone (not . isCustom) . fst)
  asum . fmap checkCustom . S.toList $ customNodes
  where
    checkCustom = \case
      CustomNode ident -> do
        neighbourhood <- typesAs <$> lookupSchema ident
        put (neighbourhood, Just ident)
        ($> (UserDefined . textify $ ident)) <$> checkTypes v
      -- TODO: Implement a safer filter to avoid having this.
      _ -> failWith . ImplementationError $ "Unreachable code: these nodes must be custom."

lookupSchema ::
  (MonadReader Schema m, MonadError ValidationError m) => Identifier -> m CompiledSchema
lookupSchema ident = do
  x <- asks $ M.lookup ident . compiledSchemata
  case x of
    Just scm -> pure scm
    Nothing -> throwError . ImplementationError $ "Unreachable state: We should be able to find this schema"

anySet :: NESet TypeNode
anySet = singleton AnyNode

textify :: Identifier -> Text
textify (Identifier t) = t

isCustom :: TypeNode -> Bool
isCustom (CustomNode _) = True
isCustom _ = False -}
