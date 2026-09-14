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
  , Specification(JsonEmbed, JsonInt, JsonLet, JsonObject, JsonString)
  , type (:::)
  )
import Data.JsonSpec.Tuple (Field(Field), TupleEncoding(toJsonStructure), encode)
import Data.Proxy (Proxy(Proxy))
import Prelude (IO, Int, print)

newtype Wrapper a = Wrapper a

instance
    HasJsonEncodingSpec (Wrapper a)
  where
    type EncodingSpec (Wrapper a) =
      JsonLet
        '[ '("Unused", JsonString) ]
        (JsonObject '[ "payload" ::: JsonEmbed (EncodingSpec a)] )

instance
    (TupleEncoding a)
  =>
    TupleEncoding (Wrapper a)
  where
    toJsonStructure (Wrapper w) = (Field @"payload" (toJsonStructure w), ())

newtype MyInt = MyInt Int
instance HasJsonEncodingSpec MyInt where
  type EncodingSpec MyInt = JsonInt
instance TupleEncoding MyInt where
  toJsonStructure (MyInt i) = i

main :: IO ()
main =
  print
    (
      encode
        (Proxy @(EncodingSpec (Wrapper MyInt)))
        (toJsonStructure (Wrapper (MyInt 1)))
    )
