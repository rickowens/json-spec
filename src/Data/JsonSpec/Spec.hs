{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.JsonSpec.Spec (
  Specification(..),
  FieldSpec(..),
  (:::),
  (::?),
  HasJsonEncodingSpec(..),
  HasJsonDecodingSpec(..),
) where

import GHC.TypeLits (Symbol)
import Prelude ()

{-|
  Simple DSL for defining type level "specifications" for JSON
  data. Similar in spirit to (but not isomorphic with) JSON Schema.

  Intended to be used at the type level using @-XDataKinds@

  Particular codecs (for example 'Data.JsonSpec.Codec.Tuple') interpret
  these specifications into concrete Haskell representations and
  encode/decode strategies.
-}
data Specification where
  JsonObject :: [FieldSpec] -> Specification
    {-^
      An object with the specified properties, each having its own
      specification. This does not yet support optional properties,
      although a property can be specified as "nullable" using
      `JsonNullable`
    -}
  JsonString :: Specification
    {-^ An arbitrary JSON string. -}
  JsonNum :: Specification
    {-^ An arbitrary (floating point) JSON number. -}
  JsonInt :: Specification
    {-^ A JSON integer.  -}
  JsonArray :: Specification -> Specification
    {-^ A JSON array of values which conform to the given spec. -}
  JsonDict :: Specification -> Specification
    {-^
      A JSON object used as a dictionary: arbitrary string keys, with every
      value conforming to the given specification.

      This is distinct from 'JsonObject', which represents a record with
      statically known fields.
    -}
  JsonBool :: Specification
    {-^ A JSON boolean value. -}
  JsonNullable :: Specification -> Specification
    {-^
      A value that can either be `null`, or else a value conforming to
      the specification.

      E.g.:

      > type SpecWithNullableField =
      >   JsonObject '[
      >     Required "nullableProperty" (JsonNullable JsonString)
      >   ]
    -}
  JsonEither :: [Specification] -> Specification
    {-^
      One of several different specifications. Corresponds to json-schema
      "oneOf". Useful for encoding sum types. Takes a type-level list of
      specs.

      Example:

      > data MyType
      >   = Foo Text
      >   | Bar Int
      >   | Baz UTCTime
      > instance HasJsonEncodingSpec MyType where
      >   type EncodingSpec MyType =
      >     JsonEither
      >       '[
      >         JsonObject '[
      >           Required "tag" (JsonTag "foo"),
      >           Required "content" JsonString
      >         ],
      >         JsonObject '[
      >           Required "tag" (JsonTag "bar"),
      >           Required "content" JsonInt
      >         ],
      >         JsonObject '[
      >           Required "tag" (JsonTag "baz"),
      >           Required "content" JsonDateTime
      >         ]
      >       ]
    -}
  JsonTag :: Symbol -> Specification
    {-^ A constant string value -}
  JsonDateTime :: Specification
    {-^
      A JSON string formatted as an ISO-8601 string. In Haskell this
      corresponds to `Data.Time.UTCTime`, and in json-schema it corresponds
      to the "date-time" format.
    -}
  JsonLet :: [(Symbol, Specification)] -> Specification -> Specification
    {-^
      A "let" expression. This is useful for giving names to types, which can
      then be used in the generated code.

      This is also useful to shorten repetitive type definitions. For example,
      this repetitive definition:

      > type Triangle =
      >   JsonObject '[
      >     Required "vertex1" (JsonObject '[
      >       Required "x" JsonInt,
      >       Required "y" JsonInt,
      >       Required "z" JsonInt
      >     ]),
      >     Required "vertex2" (JsonObject '[
      >       Required "x" JsonInt,
      >       Required "y" JsonInt,
      >       Required "z" JsonInt
      >     ]),
      >     Required "vertex3" (JsonObject '[
      >       Required "x" JsonInt),
      >       Required "y" JsonInt),
      >       Required "z" JsonInt)
      >     ])
      >   ]

      Can be written more concisely as:

      > type Triangle =
      >   JsonLet
      >     '[
      >       '("Vertex", JsonObject '[
      >          ('x', JsonInt),
      >          ('y', JsonInt),
      >          ('z', JsonInt)
      >        ])
      >      ]
      >      (JsonObject '[
      >        "vertex1" ::: JsonRef "Vertex",
      >        "vertex2" ::: JsonRef "Vertex",
      >        "vertex3" ::: JsonRef "Vertex"
      >      ])

      Another use is to define recursive types:

      > type LabelledTree =
      >   JsonLet
      >     '[
      >       '("LabelledTree", JsonObject '[
      >         "label" ::: JsonString,
      >         "children" ::: JsonArray (JsonRef "LabelledTree")
      >        ])
      >      ]
      >     (JsonRef "LabelledTree")
    -}
  JsonRef :: Symbol -> Specification
    {-^
      A reference to a specification which has been defined in a surrounding
      'JsonLet'.
    -}
  JsonRaw :: Specification
    {-^ Some raw, uninterpreted JSON value -}
  JsonAnnotated :: forall k. [(Symbol, k)] -> Specification -> Specification
    {-^
      An annotation on a specification. This is purely for documentation
      purposes and has no effect on encoding or decoding. The annotations
      are a list of key-value pairs at the type level. Keys are always
      symbols (type-level strings). Values can be any kind @k@: strings
      ('Symbol'), booleans ('Bool'), natural numbers ('Nat'), or any
      custom promoted type the user defines. Within one list, all values
      must have the same kind.

      E.g.:

      > type AnnotatedUser =
      >   JsonAnnotated
      >     '[ '("description", "A user record")
      >      , '("example", "...")
      >      ]
      >     (JsonObject '[
      >       Required "name" JsonString,
      >       Optional "last-login" JsonDateTime
      >      ])
      >
      > type ReadOnlyObject =
      >   JsonAnnotated '[ '("readOnly", 'True) ] (JsonObject '[])
    -}


{-| Specify a field in an object.  -}
data FieldSpec
  = Required Symbol Specification {-^ The field is required -}
  | Optional Symbol Specification {-^ The field is optionsl -}


{-| Alias for 'Required'. -}
type (:::) = Required


{-| Alias for 'Optional'. -}
type (::?) = Optional


{- |
  Types of this class can be associated with a type-level encoding
  'Specification'.
-}
class HasJsonEncodingSpec a where
  {- | The encoding specification. -}
  type EncodingSpec a :: Specification


{- |
  Types of this class can be associated with a type-level decoding
  'Specification'.
-}
class HasJsonDecodingSpec a where
  {- | The decoding 'Specification'. -}
  type DecodingSpec a :: Specification
