{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fdefer-type-errors -Wno-error=deferred-type-errors #-}

module Main (main) where

import Data.JsonSpec
  ( HasJsonEncodingSpec(EncodingSpec)
  , Specification
    ( JsonArray, JsonEmbed, JsonInt, JsonLet, JsonNum, JsonObject, JsonRef
    , JsonString
    )
  , type (:::)
  )
import Data.JsonSpec.Tuple
  ( Field(Field), Ref(Ref), TupleEncoding(toJsonStructure), encode
  )
import Data.Proxy (Proxy(Proxy))
import Data.Scientific (Scientific)
import Data.Text (Text, empty)
import Prelude ((.), (<$>), IO, Int, print)

data Money = Money
  { currency :: Text
  , amount :: Scientific
  }
instance HasJsonEncodingSpec Money where
  type EncodingSpec Money =
    JsonObject
      '[ "currency" ::: JsonString
       , "amount" ::: JsonNum
       ]
instance TupleEncoding Money where
  toJsonStructure money =
    ( Field money.currency
    , ( Field money.amount
      , ()))

data LineItem = LineItem
  { description :: Text
  , quantity :: Int
  , unitPrice :: Money
  , lineTotal :: Money
  }
instance HasJsonEncodingSpec LineItem where
  type EncodingSpec LineItem =
    JsonLet
      '[ '("Money", EncodingSpec Money) ]
      ( JsonObject
          '[ "description" ::: JsonString
           , "quantity" ::: JsonInt
           , "unitPrice" ::: JsonRef "Money"
           , "lineTotal" ::: JsonRef "Money"
           ]
      )
instance TupleEncoding LineItem where
  toJsonStructure li =
    ( Field li.description
    , ( Field li.quantity
      , ( Field (Ref (toJsonStructure li.unitPrice))
        , ( Field (Ref (toJsonStructure li.lineTotal))
          , ()))))

data Invoice = Invoice
  { invoiceNumber :: Text
  , items :: [LineItem]
  , featured :: [LineItem]
  }
instance HasJsonEncodingSpec Invoice where
  type EncodingSpec Invoice =
    JsonLet
      '[ '("LineItem", JsonEmbed (EncodingSpec LineItem)) ]
      (JsonObject
        '[ "invoiceNumber" ::: JsonString
         , "items" ::: JsonArray (JsonRef "LineItem")
         , "featured" ::: JsonArray (JsonRef "LineItem")
         ])
instance TupleEncoding Invoice where
  toJsonStructure inv =
    ( Field inv.invoiceNumber
    , ( Field @"items" (Ref . toJsonStructure <$> inv.items)
      , ( Field @"featured" (Ref . toJsonStructure <$> inv.featured)
        , ())))


main :: IO ()
main =
  let
    money :: Money
    money =
      Money
        { currency = empty
        , amount = 0
        }

    lineItem :: LineItem
    lineItem =
      LineItem
        { description = empty
        , quantity = 0
        , unitPrice = money
        , lineTotal = money
        }

    invoice :: Invoice
    invoice =
      Invoice
        { invoiceNumber = empty
        , items = [lineItem]
        , featured = [lineItem]
        }
  in
    print
      (encode
        (Proxy @(EncodingSpec Invoice))
        (toJsonStructure invoice))
