Roundtrip Aeson
===============

[roundstrip][1] allows you to write [invertible syntax descriptions][2] -- or,
to put it another way, a parser and pretty printer combined -- for String or
XML data. This package extends this to support constructing and destructing
JSON documents.

[1]: https://hackage.haskell.org/package/roundtrip
[2]: http://scholar.google.com/scholar?cluster=14145973580303258649

Example
-------

Using `roundtrip-aeson` is relatively straightforward:

1. Define a data type;

2. Describe the syntax of its JSON representation; and

3. Use that representation

````{.haskell}
import Data.Aeson.RoundTrip

data Invoice
    = Unpaid Bool Integer Bool
    | Paid Integer
  deriving (Show)

invoiceSyntax :: JsonSyntax s => s Invoice
invoiceSyntax =
    unpaid
        <$> jsonField "foo" jsonBool
        <*> jsonField "bar" (demote _Integer <$> jsonNumber)
        <*> jsonField "baz" jsonBool
    <|> paid
        <$> jsonField "bar" (demote _Integer <$> value)

main :: IO ()
main = do
    -- Build a JSON representation.
    let Just x = runBuilder invoiceSyntax $ Unpaid False 40 [False]
    L.putStrLn $ encode x
    -- Parse a JSON representation.
    print $ runParser invoiceSyntax x
````

See [tests/demo.hs][3] for the complete source of this example.

[3]: blob/master/tests/demo.hs
