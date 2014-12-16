{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import Control.Isomorphism.Partial
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Monoid
import Data.Text (Text)
import Text.Roundtrip.Classes

import Data.Aeson.RoundTrip

-- * Example

data Invoice
    = Unpaid Bool Integer Bool
    | Paid Integer
  deriving (Eq, Show)

defineIsomorphisms ''Invoice

data Account = Account { name :: Text, invoices :: [Invoice] }
  deriving Show

defineIsomorphisms ''Account

invoiceSyntax :: JsonSyntax s => s Invoice
invoiceSyntax =
    unpaid
        <$> jsonField "foo" jsonBool
        <*> jsonField "bar" (demote _Integer <$> jsonNumber)
        <*> jsonField "baz" jsonBool
    <|> paid
        <$> jsonField "bar" (demote _Integer <$> jsonNumber)

accountSyntax :: JsonSyntax s => s Account
accountSyntax = account
    <$> jsonField "name" jsonString
    <*> jsonField "invoices" (list <$> jsonArray invoiceSyntax)

main :: IO ()
main = do
    putStrLn "FIELDS"
    putStrLn "\tUNPARSE"
    let Just x = runBuilder invoiceSyntax $ Unpaid False 40 False
    let Just y = runBuilder invoiceSyntax $ Paid 42
    L.putStrLn $ "\t" <> encode x
    L.putStrLn $ "\t" <> encode y
    putStrLn "\n\tPARSE"
    putStrLn $ "\t" <> show (runParser invoiceSyntax x)
    putStrLn $ "\t" <> show (runParser invoiceSyntax y)

    putStrLn "\n\nLISTS"
    putStrLn "\tUNPARSE"
    let Just z1 = runBuilder accountSyntax $ Account "Foo"
            [ Unpaid False 40 False
            , Paid 42
            ]
    L.putStrLn $ "\t" <> encode z1
    let Just z2 = runBuilder accountSyntax $ Account "Bar" []
    L.putStrLn $ "\t" <> encode z2
    putStrLn "\n\tPARSE"
    putStrLn $ "\t" <> show (runParser accountSyntax z1)
    putStrLn $ "\t" <> show (runParser accountSyntax z2)
