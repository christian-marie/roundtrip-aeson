{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- TH does not generate signatures
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Control.Isomorphism.Partial
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Monoid
import Data.Text (Text)
import Text.Roundtrip.Classes
import Text.Roundtrip.Combinators

import Data.Aeson.Roundtrip

-- | A silly example
data Invoice
    = Unpaid Integer [Bool]
    | Paid Double
  deriving (Show)

defineIsomorphisms ''Invoice

-- | An example of nesting syntaxes
data Account = Account Text [Invoice]
  deriving Show

defineIsomorphisms ''Account

-- | A syntax is an abstract representation of the structure of a document.
--
-- It is built up by composing partial isomorphisms which have been
-- 'IsoFunctor' fmapped onto the 'JsonSyntax' value primitive.
invoiceSyntax :: JsonSyntax s => s Invoice
invoiceSyntax =
    -- unpaid is an iso from Invoice to (Integer, [Bool])
    unpaid
        -- Ignore "paid", but make sure it's set to False
        <$> jsonField "paid" (jsonBool `is` False)
         *> jsonField "bar" jsonIntegral
        <*> jsonField "baz" (many jsonBool)
    -- If the s Invoice above failed, try the one for paid
    <|> paid
        <$> jsonField "paid" (jsonBool `is` True)
         *> jsonField "bar" jsonRealFloat

-- | An example of nesting syntax definitions
accountSyntax :: JsonSyntax s => s Account
accountSyntax = account
    <$> jsonField "name" jsonString
    <*> jsonField "invoices" (many invoiceSyntax)

-- | A really bad acceptance test
main :: IO ()
main = do
    putStrLn "FIELDS"
    putStrLn "\tUNPARSE"

    let Just x = runBuilder invoiceSyntax $ Unpaid 40 [False, True, False]
    let Just y = runBuilder invoiceSyntax $ Paid 42

    L.putStrLn $ "\t" <> encode x
    L.putStrLn $ "\t" <> encode y

    putStrLn "\n\tPARSE"

    putStrLn $ "\t" <> show (runParser invoiceSyntax x)
    putStrLn $ "\t" <> show (runParser invoiceSyntax y)

    putStrLn "\n\nLISTS"
    putStrLn "\tUNPARSE"

    let Just z1 = runBuilder accountSyntax $ Account "Foo"
            [ Unpaid 44 [True]
            , Paid 46
            ]
    L.putStrLn $ "\t" <> encode z1

    let Just z2 = runBuilder accountSyntax $ Account "Bar" []
    L.putStrLn $ "\t" <> encode z2

    putStrLn "\n\tPARSE"

    putStrLn $ "\t" <> show (runParser accountSyntax z1)
    putStrLn $ "\t" <> show (runParser accountSyntax z2)
