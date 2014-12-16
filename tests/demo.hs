{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import Control.Isomorphism.Partial
import Control.Lens.Prism
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
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
    <*> jsonField "invoices" (list <$> (jsonArray invoiceSyntax))

main :: IO ()
main = do
    putStrLn "UNPARSE"
    let Just x = runBuilder invoiceSyntax $ Unpaid False 40 False
    let Just y = runBuilder invoiceSyntax $ Paid 42
    L.putStrLn $ encode x
    L.putStrLn $ encode y
    putStrLn "\nPARSE"
    print (runParser invoiceSyntax x)
    print (runParser invoiceSyntax y)

    putStrLn "\n\nLists"
    let Just z = runBuilder accountSyntax $ Account "Thomas" 
            [ Unpaid False 40 False
            , Paid 42
            ]
    L.putStrLn $ encode z
    print (runParser accountSyntax z)
