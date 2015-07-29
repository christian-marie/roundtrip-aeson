Roundtrip Aeson
===============

[![Build Status][6]][7]

[roundtrip][1] allows you to write [invertible syntax descriptions][2] -- or,
to put it another way, a parser and pretty printer combined -- for String or
XML data. This package extends this to support constructing and destructing
JSON documents.

Example
-------

Using `roundtrip-aeson` is relatively straightforward:

1. Define your data type;

2. Define [partial isomorphisms][3] for the constructors (probably using the
[template haskell][4]);

2. Describe the syntax of its JSON representation; and

3. Use that representation to build and parse JSON.

````{.haskell}
import Data.Aeson.RoundTrip

data Invoice
    = Unpaid Bool Integer Bool
    | Paid Double
  deriving (Show)

defineIsomorphisms ''Invoice

invoiceSyntax :: JsonSyntax s => s Invoice
invoiceSyntax =
    unpaid
        <$> jsonField "overdue" jsonBool
        <*> jsonField "total"   jsonIntegral
        <*> jsonField "warned"  jsonBool
    <|> paid
        <$> jsonField "total"   jsonRealFrac

main :: IO ()
main = do
    -- Build a JSON representation.
    let Right x = runBuilder invoiceSyntax $ Unpaid False 40 [False]
    L.putStrLn $ encode x
    -- Parse a JSON representation.
    print $ runParser invoiceSyntax x
````

See [tests/demo.hs][5] for the complete source of this example.

[1]: https://hackage.haskell.org/package/roundtrip
[2]: http://scholar.google.com/scholar?cluster=14145973580303258649
[3]: https://hackage.haskell.org/package/roundtrip/docs/Control-Isomorphism-Partial-Iso.html
[4]: https://hackage.haskell.org/package/roundtrip/docs/Control-Isomorphism-Partial-TH.html
[5]: https://github.com/anchor/roundtrip-aeson/blob/master/tests/demo.hs
[6]: https://travis-ci.org/anchor/roundtrip-aeson.svg?branch=master
[7]: https://travis-ci.org/anchor/roundtrip-aeson
