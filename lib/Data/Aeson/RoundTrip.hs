{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.Aeson.RoundTrip where

import qualified Control.Category as C
import Control.Isomorphism.Partial
import Control.Lens hiding (Iso)
import Control.Monad (mplus, (>=>), liftM2)
import Data.Aeson
import Data.Aeson.Lens
import Data.HashMap.Strict (union)
import Data.Scientific
import Data.Text (Text)
import qualified Data.Vector as V
import Text.Roundtrip.Classes

-- * Lenses, Prisms, and Isomorphisms.
--
-- | Demote a lens 'Prism' to a partial 'Iso'.
--
-- This involves strapping a _Just onto the review as a prism is slightly
-- "stronger" than an Iso anyway. Bear in mind that this is not a lens iso but
-- a RoundTrip iso.
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
jsonBool = demote _Bool <$> value

-- | Un-/parse a number JSON value.
jsonNumber :: JsonSyntax s => s Scientific
jsonNumber = demote _Number <$> value

-- | Un-/parse a string JSON value.
jsonString :: JsonSyntax s => s Text
jsonString = demote _String <$> value

-- | Un-/parse a value in a JSON object field.
jsonField
    :: JsonSyntax s
    => Text
    -> Iso Value v
    -> s v
jsonField name syntax = syntax C.. demote (keyPrism name) <$> value

-- ** Unparsing

-- | A 'JsonSyntax' which constructs JSON values.
data JsonBuilder a = JsonBuilder
    { runBuilder :: a -> Maybe Value }

instance IsoFunctor JsonBuilder where
    -- | Functor from Isos to Hask restricted to JsonBuilder
    i <$> JsonBuilder b = JsonBuilder $ unapply i >=> b

instance ProductFunctor JsonBuilder where
    -- | Join two builders together into a single builder which accepts a pair
    -- of inputs.
    JsonBuilder p <*> JsonBuilder q = JsonBuilder $ \(a,b) -> do
        ea <- p a
        eb <- q b
        merge ea eb
      where
        merge (Object a) (Object b) = Just . Object $ a `union` b
        merge (Array a) (Array b) = Just . Array $ a V.++ b
        -- We don't support the merging of top multiple top-level items, that
        -- they don't make sense.
        merge _ _ = Nothing

instance Alternative JsonBuilder where
    -- | Run one 'JsonBuilder' or, iff it fails, another.
    JsonBuilder p <||> JsonBuilder q = JsonBuilder $ \a -> p a `mplus` q a
    -- | An empty 'JsonBuilder' always fails.
    empty = JsonBuilder $ const Nothing

instance Syntax JsonBuilder where
    -- | 'JsonBuilder' which accepts only a specific value or else fails.
    pure x = JsonBuilder $ \y ->
        if x == y
            then Just Null
            else Nothing

instance JsonSyntax JsonBuilder where
    value = JsonBuilder Just 

-- | Parse a JSON 'Value' into some thing we can use.
data JsonParser a = JsonParser
    { runParser :: Value -> Maybe a }

instance IsoFunctor JsonParser where
    i <$> JsonParser p = JsonParser $ p >=> apply i

instance ProductFunctor JsonParser where
    -- | Apply two parsers, returning the results of both.
    JsonParser p <*> JsonParser q = JsonParser $ \v -> liftM2 (,) (p v) (q v)

instance Alternative JsonParser where
    -- | Apply one parser or, iff it fails, another.
    JsonParser p <||> JsonParser q = JsonParser $ \v -> p v `mplus` q v
    -- | Parser which doesn't parse anything.
    empty = JsonParser $ \_ -> Nothing

instance Syntax JsonParser where
    -- | Just return a fixed value.
    pure = JsonParser . const . Just 

instance JsonSyntax JsonParser where
    value = JsonParser Just
