{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.Aeson.RoundTrip where

import qualified Control.Category as C
import Control.Isomorphism.Partial
import Control.Lens hiding (Iso)
import qualified Control.Lens as L
import Control.Monad (guard, liftM2, mplus, (>=>))
import Data.Aeson
import Data.Aeson.Lens
import Data.HashMap.Strict (union)
import Data.Scientific
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Roundtrip.Classes

-- * Lenses, Prisms, and Isomorphisms.

-- | Demote a lens 'Prism' to a partial 'Iso'.
--
-- This involves strapping a _Just onto the review as a prism is slightly
-- "stronger" than an Iso anyway. Bear in mind that this is not a lens iso but
-- a RoundTrip iso.
--
-- This also works on lens isos, you can imagine this as :
--
--   demote :: L.Iso' a b -> Iso a b
--
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

-- | Parse and unparse JSON values.
class Syntax delta => JsonSyntax delta where

    -- | Run a parser over some other parser.
    --
    -- This can be used to, e.g., traverse the fields of an 'Object'
    -- constructor and parse the result.
    runSub :: delta v -> delta Value -> delta v

    -- | Parse any JSON value.
    value :: delta Value

    -- | Given a syntax of vs, parse a vector of them if the Value this works
    -- on is an "Array".
    jsonVector :: delta v -> delta (Vector v)

-- | Ensure that a value 'a' is "produced" or "consumed".
--
-- This is intended to be used with *> and <*
is :: (JsonSyntax delta, Eq a) => delta a -> a -> delta ()
is s a = demote (prism' (const a) (guard . (a ==))) <$> s

jsonList :: JsonSyntax s => s v -> s [v]
jsonList p =
    demote (L.iso V.toList V.fromList) <$> jsonVector p

-- | Un-/parse a boolean JSON value.
jsonBool :: JsonSyntax s => s Bool
jsonBool = demote _Bool <$> value

-- | Un-/parse a number JSON value.
jsonNumber :: JsonSyntax s => s Scientific
jsonNumber = demote _Number <$> value

-- | Un-/parse an integral number JSON value.
jsonIntegral :: (Integral a, JsonSyntax s) => s a
jsonIntegral = demote _Integral <$> value

-- | Un-/parse a floating number JSON value.
jsonRealFloat :: (RealFloat a, JsonSyntax s) => s a
jsonRealFloat = i C.. demote _Number <$> value
  where
    i = demote $ L.iso toRealFloat (fromRational . toRational)

-- | Un-/parse a string JSON value.
jsonString :: JsonSyntax s => s Text
jsonString = demote _String <$> value

-- | Un-/parse a field in a JSON object.
jsonField
    :: JsonSyntax s
    => Text
    -> s v
    -> s v
jsonField name syntax = runSub syntax (demote (keyPrism name) <$> value)

-- ** Unparsing

-- | A 'JsonSyntax' which constructs JSON values.
data JsonBuilder a = JsonBuilder
    { runBuilder :: a -> Maybe Value }

instance IsoFunctor JsonBuilder where
    i <$> JsonBuilder b = JsonBuilder $ unapply i >=> b

instance ProductFunctor JsonBuilder where
    JsonBuilder p <*> JsonBuilder q = JsonBuilder $ \(a,b) -> do
        ea <- p a
        eb <- q b
        merge ea eb
      where
        -- We don't support the merging of anything but top level objects.
        -- Anything else doesn't make sense, you can't define it as a valid
        -- JSON document.
        merge (Object a) (Object b) = Just . Object $ a `union` b
        merge _ _ = Nothing

instance Alternative JsonBuilder where
    JsonBuilder p <||> JsonBuilder q = JsonBuilder $ \a -> p a `mplus` q a

    empty = JsonBuilder $ const Nothing

instance Syntax JsonBuilder where
    pure x = JsonBuilder $ \y ->
        if x == y
            then Just Null
            else Nothing

instance JsonSyntax JsonBuilder where
    value = JsonBuilder Just

    runSub (JsonBuilder a) (JsonBuilder b) =
        JsonBuilder $ a >=> b

    jsonVector (JsonBuilder p) =
        JsonBuilder $ V.mapM p >=> (return . Array)

-- | Parse a JSON 'Value' into some thing we can use.
data JsonParser a = JsonParser
    { runParser :: Value -> Maybe a }

instance IsoFunctor JsonParser where
    i <$> JsonParser p = JsonParser $ p >=> apply i

instance ProductFunctor JsonParser where
    JsonParser p <*> JsonParser q = JsonParser $ \v -> liftM2 (,) (p v) (q v)

instance Alternative JsonParser where
    JsonParser p <||> JsonParser q = JsonParser $ \v -> p v `mplus` q v

    empty = JsonParser $ const Nothing

instance Syntax JsonParser where
    pure = JsonParser . const . Just

instance JsonSyntax JsonParser where
    value = JsonParser Just

    runSub (JsonParser a) (JsonParser b) = JsonParser $ b >=> a

    jsonVector (JsonParser p) = JsonParser $ \v ->
        case v of Array x -> V.mapM p x
                  _       -> Nothing

