{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.Aeson.RoundTrip where

import qualified Control.Category as C
import Control.Isomorphism.Partial
import Control.Lens hiding (Iso)
import Control.Monad (mplus)
import Data.Aeson
import Data.Aeson.Lens
import Data.HashMap.Strict (union)
import Data.Scientific
import Data.Text (Text)
import qualified Data.Vector as V
import Text.Roundtrip.Classes

-- * Lenses, Prisms, and Isomorphisms.
--
-- $ Oh my!

-- | Demote a lens 'Prism' to a partial 'Iso'.
demote :: Prism' a b -> Iso a b
demote p = unsafeMakeIso (preview p) (review (_Just . p))

defineIsomorphisms ''Value

-- | Prism to access a particular key in a JSON object.
--
-- Only a valid prism if we assume that isomorphism is viewed from the non-JSON
-- end of things. This forgets any context.
keyPrism :: Text -> Prism' Value Value
keyPrism k = prism' (\part -> Object [(k,part)]) (^? key k)

-- * JSON Syntax

-- | Parse and unparse from/to JSON.
class (Syntax delta) => JsonSyntax delta where
    -- | Parse a JSON value.
    value :: delta Value

-- | Un-/parse a boolean JSON value.
jsonBool :: JsonSyntax s => s Bool
jsonBool = (demote _Bool) <$> value

-- | Un-/parse a number JSON value.
jsonNumber :: JsonSyntax s => s Scientific
jsonNumber = (demote _Number) <$> value

-- | Un-/parse a string JSON value.
jsonString :: JsonSyntax s => s Text
jsonString = (demote _String) <$> value

-- | Un-/parse a value in a JSON object field.
jsonField
    :: JsonSyntax s
    => Text
    -> Iso Value v
    -> s v
jsonField name syntax = (syntax C.. (demote $ keyPrism name)) <$> value

-- ** Unparsing

-- | A 'JsonSyntax' which constructs JSON values.
data JsonBuilder a = JsonBuilder
    { runBuilder :: a -> Maybe Value }

instance IsoFunctor JsonBuilder where
    (<$>) = builderApply

instance ProductFunctor JsonBuilder where
    (<*>) = builderConcat

instance Alternative JsonBuilder where
    (<|>) = builderAlternative
    (<||>) = builderAlternative
    empty = builderEmpty

instance Syntax JsonBuilder where
    pure = builderPure

instance JsonSyntax JsonBuilder where
    value = JsonBuilder $ \v -> Just v

-- | Apply an 'Iso' to values passing into a 'JsonBuilder'.
builderApply
    :: Iso a b
    -> JsonBuilder a
    -> JsonBuilder b
builderApply i (JsonBuilder b) = JsonBuilder $ \v ->
    case unapply i v of
        Just  x -> b x
        Nothing -> Nothing

-- | Join two builders together into a single builder which accepts a pair of
-- inputs.
builderConcat
    :: JsonBuilder a
    -> JsonBuilder b
    -> JsonBuilder (a,b)
builderConcat (JsonBuilder p) (JsonBuilder q) = JsonBuilder $ \(a,b) -> do
    ea <- p a
    eb <- q b
    merge ea eb
  where
    merge (Object a) (Object b) = Just . Object $ a `union` b
    merge (Array a) (Array b) = Just . Array $ a V.++ b
    merge _ _ = Nothing

-- | Run one 'JsonBuilder' or, iff it fails, another.
builderAlternative
    :: JsonBuilder a
    -> JsonBuilder a
    -> JsonBuilder a
builderAlternative (JsonBuilder p) (JsonBuilder q) =
    JsonBuilder $ \a -> p a `mplus` q a

-- | An empty 'JsonBuilder' always fails.
builderEmpty
    :: JsonBuilder a
builderEmpty = JsonBuilder $ \_ -> Nothing

-- | 'JsonBuilder' which accepts only a specific value or else fails.
builderPure
    :: (Eq a)
    => a
    -> JsonBuilder a
builderPure x = JsonBuilder $ \y ->
    if x == y
        then return Null
        else Nothing

-- ** Parsing

-- | Parse a JSON 'Value' into some thing we can use.
data JsonParser a = JsonParser
    { runParser :: Value -> Maybe a }

instance IsoFunctor JsonParser where
    (<$>) = parserApply

instance ProductFunctor JsonParser where
    (<*>) = parserConcat

instance Alternative JsonParser where
    (<|>) = parserAlternative
    (<||>) = parserAlternative
    empty = parserEmpty

instance Syntax JsonParser where
    pure = parserPure

instance JsonSyntax JsonParser where
    value = JsonParser $ \v -> Just v

-- | Apply an 'Iso' over a 'JsonBuilder'.
parserApply
    :: Iso a b
    -> JsonParser a
    -> JsonParser b
parserApply i (JsonParser p) = JsonParser $ \v -> do
    a <- p v
    apply i a

-- | Apply two parsers, returning the results of both.
parserConcat
    :: JsonParser a
    -> JsonParser b
    -> JsonParser (a,b)
parserConcat (JsonParser p) (JsonParser q) = JsonParser $ \v -> do
    a <- p v
    b <- q v
    return (a,b)

-- | Apply one parser or, iff it fails, another.
parserAlternative
    :: JsonParser a
    -> JsonParser a
    -> JsonParser a
parserAlternative (JsonParser p) (JsonParser q) = JsonParser $ \v -> do
    p v `mplus` q v

-- | Parser which doesn't parse anything.
parserEmpty
    :: JsonParser a
parserEmpty = JsonParser $ \_ -> Nothing

-- | Just return a fixed value.
parserPure
    :: (Eq a)
    => a
    -> JsonParser a
parserPure x = JsonParser $ \_ -> return x
