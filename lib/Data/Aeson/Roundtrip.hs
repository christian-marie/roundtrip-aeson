{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- TH does not generate signatures
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Aeson.Roundtrip
(
    -- * Parser/Builder
    JsonParser(..),
    JsonBuilder(..),
    -- * Combinators
    is,
    wat,
    -- * Syntaxes
    jsonField,
    jsonString,
    jsonBool,
    jsonNumber,
    jsonIntegral,
    jsonRealFloat,
    -- * Lenses, Prisms, and Isomorphisms.
    demote,
    demoteLR,
    demoteL,
    demoteR,
    -- * JSON Syntax
    JsonSyntax (..)
)
where

import Control.Category ((.))
import Control.Isomorphism.Partial
import Control.Lens hiding (Iso)
import qualified Control.Lens as L
import Control.Monad (guard, liftM2, mplus, (>=>))
import Data.Aeson
import Data.Aeson.Lens
import Data.HashMap.Strict (union)
import Data.Monoid
import Data.Scientific
import Data.Text (Text)
import Data.Vector ((!?))
import qualified Data.Vector as V
import Prelude hiding ((.))
import Text.Roundtrip.Classes


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
demote :: String -> Prism' a b -> Iso a b
demote name p = unsafeMakeNamedIso name (preview p) (review (_Just . p))

-- | Demote something with show instances for better messages.
demoteLR :: (Show a, Show b) => String -> Prism' a b -> Iso a b
demoteLR name p = unsafeMakeNamedIsoLR name (preview p) (review (_Just . p))

demoteL :: (Show a) => String -> Prism' a b -> Iso a b
demoteL name p = unsafeMakeNamedIsoL name (preview p) (review (_Just . p))

demoteR :: (Show b) => String -> Prism' a b -> Iso a b
demoteR name p = unsafeMakeNamedIsoR name (preview p) (review (_Just . p))

-- | Parse and unparse JSON values.
class Syntax s => JsonSyntax s where

    -- | Run a parser over some other parser.
    --
    -- This can be used to, e.g., traverse the fields of an 'Object'
    -- constructor and parse the result.
    runSub :: s v -> s Value -> s v

    -- | Parse any JSON value.
    value :: s Value

-- | Ensure that a value 'a' is "produced" or "consumed".
--
-- This is intended to be used infix in conjunction with *> and <*
is :: (JsonSyntax s, Eq a) => s a -> a -> s ()
is s a = demoteR "is" (prism' (const a) (guard . (a ==))) <$> s

-- | With Arbitrary Thing: Given a thing, ensure that it is always included on
-- the way "back" from JSON, but never ends up in the JSON document.
--
-- This is almost like pure, going one way.
wat :: (JsonSyntax s) => a -> s a
wat a = demoteL "wat"
                (prism' (const $ Object mempty) (const $ Just a)) <$> value

-- | Un-/parse from within a field in a JSON object.
jsonField
    :: JsonSyntax s
    => Text
    -- ^ Key to lookup/insert
    -> s v
    -- ^ Sub-parser
    -> s v
jsonField k syntax = runSub syntax (keyIso <$> value)
  where
    -- Only valid if we assume that isomorphism is viewed from the non-JSON end
    -- of things. This forgets any context.
    keyIso = demoteLR ("key " <> show k) $ prism' (\part -> Object [(k,part)]) (^? key k)

-- | Un-/parse a boolean JSON value.
jsonBool :: JsonSyntax s => s Bool
jsonBool = demoteLR "jsonBool" _Bool <$> value

-- | Un-/parse a number JSON value.
jsonNumber :: JsonSyntax s => s Scientific
jsonNumber = demoteLR "jsonNumber" _Number <$> value

-- | Un-/parse an integral number JSON value.
jsonIntegral :: (Integral a, JsonSyntax s) => s a
jsonIntegral = demoteL "jsonIntegral" _Integral <$> value

-- | Un-/parse a floating number JSON value.
jsonRealFloat :: (RealFloat a, JsonSyntax s) => s a
jsonRealFloat = i . demoteL "jsonRealFloat (number)" _Number <$> value
  where
    i = demoteL "jsonRealFloat (toRealFloat)" $
            L.iso toRealFloat (fromRational . toRational)

-- | Un-/parse a string JSON value.
jsonString :: JsonSyntax s => s Text
jsonString = demoteLR "String" _String <$> value

-- | Try to apply an iso, provide message on failure
tryLR :: Iso a b -> a -> Either String b
tryLR i a =
    case isoLR i a of
        Just x -> Right x
        Nothing -> Left $ isoFailedErrorMessageL i a

-- | Try to unapply an iso, provide message on failure
tryRL :: Iso a b -> b -> Either String a
tryRL i b =
    case isoRL i b of
        Just x -> Right x
        Nothing -> Left $ isoFailedErrorMessageR i b

-- | An implementation of 'JsonSyntax' which constructs JSON values.
newtype JsonBuilder a = JsonBuilder
    { runBuilder :: a -> Either String Value }

instance IsoFunctor JsonBuilder where
    -- When going from a to 'Value' we simply want to compose the possible iso
    -- failures in the 'unapply' direction.
    i <$> JsonBuilder b = JsonBuilder $ tryRL i >=> b

instance ProductFunctor JsonBuilder where
    -- When building a 'Value' we want to decompose our church pair list tupled
    -- builders and merge the results together.
    --
    -- Note that the second argument is not pattern matched, this is to ensure
    -- that it is not eagerly constructed and does not diverge in things like
    -- many.
    JsonBuilder p <*> JsonBuilder q = JsonBuilder $ \(a,b) -> do
        a' <- p a
        b' <- q b
        merge a' b'
      where
        -- Merging of two objects is simply a union, this rule fires when you
        -- do things like:
        --
        -- jsonField "a" p <*> jsonField "b" p
        merge (Object a) (Object b) = Right . Object $ a `union` b
        -- merge Null (Object b) = Object b
        -- Merging of head and tail of arrays, this rule fires when using
        -- things like the many combinator to create a JSON array
        merge a (Array b) = Right . Array $ V.cons a b
        merge x Null = Right x
        merge Null x = Right x
        merge x y = Left $
            "Don't know how to merge: " ++ show x ++ " <*> " ++ show y

instance Alternative JsonBuilder where
    -- Try the left first, then right.
    JsonBuilder p <||> JsonBuilder q = JsonBuilder $ \a -> p a `mplus` q a

    -- Always Left
    empty = JsonBuilder . const $ Left "empty"

instance Syntax JsonBuilder where
    -- | Have to rewrite Null as [] as pure () is will make a Null as it
    -- terminates the list.
    --
    -- This is so that pure can make nulls, which is "nicer" for things like
    -- optional.
    rule "many" _ (JsonBuilder b) =
        JsonBuilder $ b >=> (\case Null -> Right $ Array mempty
                                   x    -> Right x)
    rule _ _ x = x

    pure x = JsonBuilder $ \y ->
        if x == y
            then Right Null
            else Left "pure, x /= y"

instance JsonSyntax JsonBuilder where
    -- | To roduces a 'Value', we simply need to pass it through.
    value = JsonBuilder Right

    -- Run a sub-parser. Just composition, really.
    runSub (JsonBuilder a) (JsonBuilder b) =
        JsonBuilder $ a >=> b


-- | An implementation of 'JsonSyntax' which deconstructs JSON values.
newtype JsonParser a = JsonParser
    { runParser :: Value -> Either String a }

instance IsoFunctor JsonParser where
    -- The opposite of a JsonParser in both order of composition and direction
    -- of iso
    i <$> JsonParser p = JsonParser $ p >=> tryLR i

instance ProductFunctor JsonParser where
    -- When coming from a 'Value' we either want to tuple things up, or, in
    -- the special case of a list, consume the head and pass the tail on. This
    -- is a simple way of getting the many combinator to work on JSON.
    JsonParser p <*> JsonParser q = JsonParser f
      where
        f v | Array x <- v, Just y <- x !? 0
            = liftM2 (,) (p y) (q . Array $ V.tail x)
            | Array _ <- v
            = Left "Empty array"
            | otherwise
            = liftM2 (,) (p v) (q v)

instance Alternative JsonParser where
    JsonParser p <||> JsonParser q = JsonParser $ \v -> p v `mplus` q v

    empty = JsonParser . const $ Left "empty"

instance Syntax JsonParser where
    pure = JsonParser . const . Right

instance JsonSyntax JsonParser where
    value = JsonParser Right

    runSub (JsonParser a) (JsonParser b) = JsonParser $ b >=> a
