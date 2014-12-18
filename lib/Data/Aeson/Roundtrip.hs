{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- TH does not generate signatures
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Aeson.Roundtrip where

import qualified Control.Category as C
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

-- * JSON Syntax

-- | Parse and unparse JSON values.
class Syntax s => JsonSyntax s where

    -- | Run a parser over some other parser.
    --
    -- This can be used to, e.g., traverse the fields of an 'Object'
    -- constructor and parse the result.
    runSub :: s v -> s Value -> s v

    -- | Parse any JSON value.
    value :: s Value

-- * Combinators

-- | Ensure that a value 'a' is "produced" or "consumed".
--
-- This is intended to be used infix in conjunction with *> and <*
is :: (JsonSyntax s, Eq a) => s a -> a -> s ()
is s a = demote (prism' (const a) (guard . (a ==))) <$> s

-- | With Arbitrary Thing: Given a thing, ensure that it is always included on
-- the way "back" from JSON, but never ends up in the JSON document.
--
-- This is almost like pure, going one way.
wat :: JsonSyntax s => a -> s a
wat a = demote (prism' (const $ Object mempty) (const $ Just a)) <$> value

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
    keyIso = demote $ prism' (\part -> Object [(k,part)]) (^? key k)

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

-- ** Parsing and unparsing

-- | An implementation of 'JsonSyntax' which constructs JSON values.
data JsonBuilder a = JsonBuilder
    { runBuilder :: a -> Maybe Value }

instance IsoFunctor JsonBuilder where
    -- When going from a to 'Value' we simply want to compose the possible iso
    -- failures in the 'unapply' direction.
    i <$> JsonBuilder b = JsonBuilder $ unapply i >=> b

instance ProductFunctor JsonBuilder where
    -- When building a 'Value' we want to decompose our church pair list tupled
    -- builders and merge the results together.
    --
    -- Note that the second argument is not pattern matched, this is to ensure
    -- that it is not eagerly constructed and does not diverge in things like
    -- many.
    JsonBuilder p <*> q = JsonBuilder $ \(a,b) -> do
        a' <- p a
        b' <- runBuilder q b
        merge a' b'
      where
        -- Merging of two objects is simply a union, this rule fires when you
        -- do things like:
        --
        -- jsonField "a" p <*> jsonField "b" p
        merge (Object a) (Object b) = Just . Object $ a `union` b
        -- Merging of head and tail of arrays, this rule fires when using
        -- things like the many combinator to create a JSON array
        merge a (Array b) = Just . Array $ V.cons a b
        merge _ _ = Nothing

instance Alternative JsonBuilder where
    -- Try the left first, then right.
    JsonBuilder p <||> JsonBuilder q = JsonBuilder $ \a -> p a `mplus` q a

    -- Always fail
    empty = JsonBuilder $ const Nothing

instance Syntax JsonBuilder where
    pure x = JsonBuilder $ \y ->
        if x == y
            then Just $ Array []
            else Nothing

instance JsonSyntax JsonBuilder where
    -- | To roduces a 'Value', we simply need to pass it through.
    value = JsonBuilder Just

    -- Run a sub-parser. Just composition, really.
    runSub (JsonBuilder a) (JsonBuilder b) =
        JsonBuilder $ a >=> b


-- | An implementation of 'JsonSyntax' which deconstructs JSON values.
data JsonParser a = JsonParser
    { runParser :: Value -> Maybe a }

instance IsoFunctor JsonParser where
    -- The opposite of a JsonParser in both order of composition and direction
    -- of iso
    i <$> JsonParser p = JsonParser $ p >=> apply i

instance ProductFunctor JsonParser where
    -- When coming from a 'Value' we either want to tuple things up, or, in
    -- the special case of a list, consume the head and pass the tail on. This
    -- is a simple way of getting the many combinator to work on JSON.
    JsonParser p <*> q = JsonParser $ \v -> do
        let (a,b) | Array x <- v, Just y <- x !? 0 = (y, Array $ V.tail x)
                  | Array _ <- v                   = (Null, Null)
                  | otherwise                      = (v,v)
        liftM2 (,) (p a) (runParser q b)

instance Alternative JsonParser where
    JsonParser p <||> JsonParser q = JsonParser $ \v -> p v `mplus` q v

    empty = JsonParser $ const Nothing

instance Syntax JsonParser where
    pure = JsonParser . const . Just

instance JsonSyntax JsonParser where
    value = JsonParser Just

    runSub (JsonParser a) (JsonParser b) = JsonParser $ b >=> a
