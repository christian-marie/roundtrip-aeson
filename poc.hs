{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Isomorphism.Partial
import           Control.Lens                hiding (Iso)
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy        as L
import           Data.Text
import           Text.Roundtrip.Classes

-- * Lens

demote :: Prism' a b -> Iso a b
demote p = unsafeMakeIso (preview p) (review (_Just . p))

-- * JSON Syntax

-- | Parse and unparse from/to JSON.
class (Syntax delta) => JsonSyntax delta where
    field :: Text -> delta Value

-- | Apply a 'JsonSyntax' un-/parser as a sub-field.
jsonField
    :: (JsonSyntax s)
    => Text
    -> s v
    -> s v
jsonField name p = error "jsonField: not implemented"

-- | 
jsonList
    :: (JsonSyntax s)
    => s v
    -> s [v]
jsonList p = error "jsonList: not implemented"

boolField
    :: JsonSyntax s
    => Text
    -> s Bool
boolField name = jsonField name bool

integerField
    :: JsonSyntax s
    => Text
    -> s Integer
integerField name = jsonField name integer

-- | Un-/parse a boolean value.
bool
    :: (JsonSyntax s)
    => s Bool
bool = _Bool <$> pure

integer
    :: JsonSyntax s
    => s Integer
integer = undefined

-- ** Unparsing

-- | A 'JsonSyntax' which constructs JSON values.
--
-- FIXME: We probably want to specialise the 'r' type parameter to be 'Value'
-- (or, alternatively, call it DocumentBuilder and support other formats too).
data JsonBuilder m r a = JsonBuilder
    { runBuilder :: a -> m (Maybe r) }

instance (Monad m) => IsoFunctor (JsonBuilder m r) where
    (<$>) = builderApply

instance (Monad m) => ProductFunctor (JsonBuilder m r) where
    (<*>) = builderConcat

instance (Monad m) => Alternative (JsonBuilder m r) where
    (<|>) = builderAlternative
    (<||>) = builderAlternative
    empty = builderEmpty

instance (Monad m) => Syntax (JsonBuilder m r) where
    pure = builderPure

instance (Monad m) => JsonSyntax (JsonBuilder m r) where
    field = pure

-- | Apply an 'Iso' to values passing into a 'JsonBuilder'.
builderApply
    :: (Monad m)
    => Iso a b
    -> JsonBuilder m r a
    -> JsonBuilder m r b
builderApply iso (JsonBuilder b) = JsonBuilder $ \v ->
    case unapply iso v of
        Just  x -> b x
        Nothing -> return Nothing

-- | Join two builders together into a single builder which accepts a pair of
-- inputs.
builderConcat
    :: (Monad m)
    => JsonBuilder m r a
    -> JsonBuilder m r b
    -> JsonBuilder m r (a,b)
builderConcat (JsonBuilder p) (JsonBuilder q) = JsonBuilder $ \(a,b) -> do
    ma <- p a
    case ma of
        Nothing -> return Nothing
        Just !ea -> do
            mb <- q b
            case mb of
                Nothing -> return Nothing
                Just !eb -> return . Just $ ea -- `mappend` eb)

-- | Run one 'JsonBuilder' or, iff it fails, another.
builderAlternative
    :: (Monad m)
    => JsonBuilder m r a
    -> JsonBuilder m r a
    -> JsonBuilder m r a
builderAlternative (JsonBuilder p) (JsonBuilder q) = JsonBuilder $ \a -> do
    ma <- p a
    case ma of
        Nothing -> q a
        Just _  -> return ma

-- | An empty 'JsonBuilder' always fails.
builderEmpty
    :: (Monad m)
    => JsonBuilder m r a
builderEmpty = JsonBuilder $ \_ -> return Nothing

-- | 'JsonBuilder' which accepts only a specific value or else fails.
builderPure
    :: (Monad m, Eq a)
    => a
    -> JsonBuilder m r a
builderPure x = JsonBuilder $ \y ->
    if x == y
        then return . Just $ Null
        else return Nothing

-- ** Parsing

-- | Parse a JSON 'Value' into some thing we can use.
data JsonParser m r a = JsonParser
    { runParser :: r -> m (Maybe a) }

instance (Monad m) => IsoFunctor (JsonParser m r) where
    (<$>) = parserApply

instance (Monad m) => ProductFunctor (JsonParser m r) where
    (<*>) = parserConcat

instance (Monad m) => Alternative (JsonParser m r) where
    (<|>) = parserAlternative
    (<||>) = parserAlternative
    empty = parserEmpty

instance (Monad m) => Syntax (JsonParser m r) where
    pure = parserPure

instance (Monad m) => JsonSyntax (JsonParser m r) where
    field = pure

-- | Apply an 'Iso' over a 'JsonBuilder'.
parserApply
    :: (Monad m)
    => Iso a b
    -> JsonParser m r a
    -> JsonParser m r b
parserApply iso (JsonParser p) = JsonParser $ \v -> do
    a <- p v
    case a of
        Nothing -> return Nothing
        Just x -> return $ apply iso x

-- | Apply two parsers, returning the results of both.
parserConcat
    :: (Monad m)
    => JsonParser m r a
    -> JsonParser m r b
    -> JsonParser m r (a,b)
parserConcat (JsonParser p) (JsonParser q) = JsonParser $ \v -> do
    ma <- p v
    case ma of
        Nothing -> return Nothing
        Just a -> do
            mb <- q v
            case mb of
                Nothing -> return Nothing
                Just b -> return . Just $ (a,b)

-- | Apply one parser or, iff it fails, another.
parserAlternative
    :: (Monad m)
    => JsonParser m r a
    -> JsonParser m r a
    -> JsonParser m r a
parserAlternative (JsonParser p) (JsonParser q) = JsonParser $ \v -> do
    ma <- p v
    case ma of
        Just !a -> return . Just $ a
        Nothing -> do
            mb <- q v
            return mb

-- | Parser which doesn't parse anything.
parserEmpty
    :: (Monad m)
    => JsonParser m r a
parserEmpty = JsonParser $ \_ -> return Nothing

-- |
parserPure
    :: (Monad m, Eq a)
    => a
    -> JsonParser m r a
parserPure x = JsonParser $ \v ->
    return $ Just x


-- * Example

data Invoice
    = Unpaid Bool Integer Bool
    | Paid Integer
  deriving (Show)

defineIsomorphisms ''Invoice

invoiceSyntax :: JsonSyntax s => s Invoice
invoiceSyntax =
    unpaid
        <$> boolField "foo"
        <*> integerField "bar"
        <*> boolField "baz"
    <|> paid
        <$> integerField "bar"

main :: IO ()
main = do
    putStrLn "UNPARSE"
    let Just x = runBuilder invoiceSyntax $ Unpaid False 40 False
    let Just y = runBuilder invoiceSyntax $ Paid 42
    L.putStrLn $ encode x
    L.putStrLn $ encode y
    putStrLn "PARSE"
    print (runParser invoiceSyntax x)
    print (runParser invoiceSyntax y)
