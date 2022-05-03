# aeson-quick

[![hackage](https://img.shields.io/hackage/v/aeson-quick.svg)](https://hackage.haskell.org/package/aeson-quick) [![ci](https://github.com/ssadler/aeson-quick/actions/workflows/haskell.yml/badge.svg)](https://github.com/ssadler/aeson-quick/tree/0.2.0)

`aeson-quick` is a small DSL on top of [aeson](https://hackage.haskell.org/package/aeson) for casual and concise JSON construction and deconstruction.

In essence, it allows you to remove the Objects in a json `Value` using a simple key lookup syntax, so that the `FromJSON` typeclass can do the rest of the work, and it can go the other way, too.

`aeson-quick` structures can be defined using the `fromString` instance, or using the QuasiQuoter `quick` for compile time validation, ie: `[quick|{foo}|]`.

## Syntax

## Transformations

| Operation                       | Json Value                      | Quick expression &nbsp; &nbsp; | Haskell type     |
|---------------------------------|---------------------------------|----------------------|------------------|
| Parse any value                 | ``*``                           | ``.``                | ``a``            |
| Key lookup                      | ``{"foo": *}``                  | ``{foo}``            | ``a``            |
| Optional key                    | ``{}``                          | ``{foo?}``           | ``Maybe a``      |
| Multiple keys                   | ``{"foo": *, "bar": *}``        | ``{foo,bar}``        | ``(a, b)``       |
| Nested object                   | ``{"foo": {"bar": *}}``         | ``{foo:{bar}}``      | ``b``            |
| Complex object                  | ``[{"foo": *, "bar": [*, *]}]`` &nbsp; &nbsp; | ``[{a,b}]``      | ``[(a, [c])]``   |
| Array of objects                | ``[{"foo": *}]``                | ``[{foo}]``              | ``[a]``          |
| Array lookup (deconstruct only) | ``[*, *]``                      | ``[.]1``             | ``a``            |
| Array range (deconstruct only)  | ``[*, *]``                      | ``[.]1-``            | ``[a]``          |

## Examples:

#### Deconstruction:

```Haskell
> let value = [jsonlit|{"foo": true, "bar": [0, 1], "baz": [{"foo": true}, {}, {"foo": false}]}|]

> value .! "{foo}" :: Bool
True

> value .! "{foo, bar}" :: (Bool, [Int])
(True, [0, 1])

> value .! "{baz:[{foo?}]}" :: [Maybe Bool]
[Just True, Nothing, Just False]

-- value is [1,2,3]
> value .! "[.]1" :: Int
2
> value .! "[.]1-" :: [Int]
[2,3]
> value .! "[.]0-2" :: [Int]
[1,2]
> value .! "[.]4" :: Maybe Int
Nothing
```

#### Construction:

```Haskell

> "." .% True
true

> "{foo}" .% True
{"foo": true}

> "[{foo}]" .% [1,2,3]
[{"foo":1},{"foo":2},{"foo":3}]

> "{foo:[{bar?}]}" .% [Just 1, Nothing]
{"foo":[{"bar":1},{}]}
```


## Performance

Performance is extremely similar to using Aeson functions directly. See Writeup.md for more details. Benchmarks are also included.
