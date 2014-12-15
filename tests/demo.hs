{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Isomorphism.Partial
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8  as L
import           Text.Roundtrip.Classes

import           Data.Aeson.RoundTrip

-- * Example

data Invoice
    = Unpaid Bool Integer Bool
    | Paid Integer
  deriving (Show)

defineIsomorphisms ''Invoice

invoiceSyntax :: JsonSyntax s => s Invoice
invoiceSyntax =
    unpaid
        <$> jsonField "foo" (demote _Bool)
        <*> jsonField "bar" (demote _Integer)
        <*> jsonField "baz" (demote _Bool)
    <|> paid
        <$> jsonField "bar" (demote _Integer)

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
