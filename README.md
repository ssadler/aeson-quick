# aeson-quick

DSL on top of [aeson](https://hackage.haskell.org/package/aeson) to make working with JSON even easier.

[![hackage](https://img.shields.io/hackage/v/aeson-quick.svg)](https://hackage.haskell.org/package/aeson-quick)
[![travis](https://travis-ci.org/libscott/aeson-quick.svg?branch=master)](https://travis-ci.org/libscott/aeson-quick/)

Just about every project I work on these days includes handling JSON somewhere or other. It's everywhere.

When using dynamic languages you don't really have to think much about JSON, since dynamic languages have native dynamic data structures that more or less map 1:1 with JSON. Python has `dict`, Ruby has `Hash` etc.

However in static languages, type information is discarded at compile time, so if you want to support dynamic data you need a data type that can accomodate it explicitly.

In Haskell, the most widely used library for JSON is [Aeson](https://hackage.haskell.org/package/aeson), written by the author of [Real World Haskell](http://book.realworldhaskell.org/) and a whole lot of other Haskell libraries besides. Aeson provides a [Value](https://hackage.haskell.org/package/aeson-1.1.0.0/docs/Data-Aeson.html#t:Value) type to contain JSON data, and a set of tools for converting between the generic `Value` and application specific data types. It also provides blazing fast encoding and decoding of JSON data, but that's besides the point of this blog post. This post is about the process of going from Aeson's `Value` to `[YourDataType]`.

## The Problem

Aeson provides a typeclass `FromJSON` and many instances to translate `Value` data into common data types. In this way, Aeson makes it basically free to go between `Value` and types that can be represented by Haskell's type system, by telling it what type of thing to try to read. For instance:

```Haskell
> decode "[\"Duck\", [\"has beak\", \"quacks\"]]" :: Maybe (Text, [Text])
Just ("Duck",["has beak","quacks"])
```

Hooray! That was very easy. But, the JSON payload is not typical; the data it contains is not described, which is one thing that JSON objects do:

```json
{
  "animal": "Duck",
  "features": ["has beak", "quacks"]
}
```

Suddenly, the type system cannot describe the information anymore, because it does not have a way to express keys. Aeson is able to decode a JSON object into a [Map](https://hackage.haskell.org/package/containers-0.5.9.1/docs/Data-Map-Strict.html), which could accomodate the `animal` and `feature` keys in the example above, but since the values are not a single type, it doesn't take us all the way. The language provided by Aeson to do this manually is the following:

```haskell
> let Just animalValue = decode "{\"animal\":\"Duck\",\"features\":[\"has beak\",\"quacks\"]}" :: Maybe Value
> let parseAnimal = withObject "" $ \o -> (,) <$> o .: "animal" <*> o .: "features"
> parseMaybe parseAnimal animalValue :: Maybe (Text, [Text])
Just ("Duck",["has beak","quacks"])
```

Aeson does provide functions to decode and encode data types which are instances of `Generic`, but that doesn't neccesarily help (especially if we just want to do something quick and dirty!). If you're writing a method to parse the JSON into your own type, you still need to write the parser: `withObject "" $ \o -> (,) <$> o .: "animal" <*> o .: "features"`.

Well, that's not so bad, I hear you say. One line of code, a bit noisy perhaps but nothing too terrible. True, but it doesn't scale very well. Consider the following, fairly typical data structure, descibing heart attack inducing baked goods:

```json
[
  {
    "id": "0001",
    "name": "Cake",
    "ppu": 0.55,
    "batters": [
      { "id": "1001", "type": "Regular" },
      { "id": "1002", "type": "Magical" }
    ],
    "topping": [
      { "id": "5001", "type": "Chocolate" }
    ]
  }
]
```

Now it's a tiny bit hairier:

```haskell
type BakedGoods = [(Text,Text,Float,[(Text,Text)],[(Text,Text)])]

parseBakery :: Value -> Parser BakedGoods
parseBakery = parseJSON >=> mapM (\o ->
  (,,,) <$> o .: "id" <*> o .: "ppu" <*> batters o <*> toppings o)
  where
    inner o = (,) <$> o .: "id" <*> o .: "type"
    batters = (.:"batters") >=> mapM inner
    toppings = (.:"topping") >=> mapM inner
```

Up to ~6 lines of code, and we have capitalised on a duplicated structure in our input. All that for a few object lookups? Yep, sadly the type system has nothing to offer us here.

If only there were some way to provide the keys separately so that the type system could do more of the work?

## Enter `aeson-quick`

[aeson-quick](https://hackage.haskell.org/package/aeson-quick) provides a DSL which looks a bit like JSON and describes which keys to pull out of a `Value`:

```haskell
> cakeValue .! "[{id,name,ppu,batters:[{id,type}],topping:[{id,type}]}]" :: BakedGoods
[("0001","Cake",0.55,[("1001","Regular"),("1002","Magical")],[("5001","Chocolate")])]
```

What happens is, the `FromString` instance decodes the DSL expression into it's own internal `Structure` representation, and then `.!` executes that against the provided Aeson `Value`. All it's doing is traversing the `Value` according to the provided structure and essentially turning JSON objects into arrays, so that Aeson's FromJSON typeclass can do the rest.

## Structure DSL

It might seem a little tricky to understand whats going on in the above expression but it's not too complicated: In order to translate between the DSL and a Haskell type, do the following:

1. Remove any keys that have a colon `:` after them 
1. Turn the objects `{a,b}` into tuples `(a,b)`
1. Replace all the keys with the Haskell type that you want parsed (anything with a `FromJSON` instance).

So, given the example above:

```
Start with:      [{id,name,ppu,batters:[{id,type}],topping:[{id,type}]}]
Remove colons:   [{id,name,ppu,        [{id,type}],        [{id,type}]}]
Tuplify objects: [(id,name,ppu,        [(id,type)],        [(id,type)])]
Haskell types:   [(Text,Text,Float,    [(Text,Text)],      [(Text,Text)])]
```

Basically, `parseJSON` is called on any value that is a leaf node in the structure.

JSON objects of one member get discarded entirely, since there is no "tuple of 1":

```haskell
> cakeValue .! [{name}] :: [Text]
["Cake"]
```

So now, if we just want to do a quick and dirty JSON extraction of our duck friend with no fancy stuff, we can do it more easily and clearly at the same time:

```haskell
> animalValue .! "{name,features}" :: (Text,[Text])
("Duck",["has beak","quacks"])
```

Easy!

## Creating Values

As well as getting data out of a `Value`, `aeson-quick` can take some of the pain out of creating and updating a `Value`.

Simply provide the structure you want to create and corresponding data. The same process as extracting but in reverse.

If you provide `Null` as the value to be updated, the whole structure will be created for you:

```haskell
> C8.putStrLn $ encode $ "{my,complex:[{data,structure}]}" .% (1,[(10,(100,101))])
{"my":1,"complex":[{"data":10,"structure":[100,101]}]}
```

It's also smart enough to unroll lists:

```haskell
> C8.putStrLn $ encode $ "{my,complex:[{data,structure}]}" .% (1,[(10,True),(20,False)])
{"my":1,"complex":[{"structure":true,"data":10},{"structure":false,"data":20}]}
```

Already existing values can be updated:

```haskell
> let v = "{lazy}" .% True
> C8.putStrLn $ encode v
{"lazy":true}
> C8.putStrLn $ encode $ build "{duck,tea}" v ("Saxony","Green")
{"lazy":true,"duck":"Saxony","tea":"Green"}
```

## Lens

Some of you may have noticed similarities with lenses; it's true, `aeson-quick` is kind of like a specialised lens library for JSON.

But, you can also combine the two:

```haskell 
queLens :: (FromJSON a, ToJSON a) => Structure -> Lens' Value a
queLens s = lens (.!s) (build s)                                 

> let l = queLens "{animal,features}" :: Lens' Value (Text,[Text])
> animalValue ^. l . _2
["has beak", "quacks"]
> C8.putStrLn $ encode $ animalValue & l . _2 %~ ("floats":)
{"animal":"Duck","features":["floats","has beak","quacks"]}
```

There's lots room for improvement here, since my lens-fu is limited. Integration with [microlens-aeson](https://hackage.haskell.org/package/microlens-aeson) would be nice.

## Performance

We want nice syntactic sugar and we want it to perform, yes? There are some benchmarks which show that `aeson-quick` is within spitting distance of, or in some cases outperforming hand written Aeson parsers:

```
quickGetSimple       99.03 ns   (97.58 ns .. 101.8 ns) 
aesonGetSimple       62.23 ns   (61.99 ns .. 62.49 ns)
microLensGetSimple   70.16 ns   (63.64 ns .. 82.61 ns)
quickGetComplex      3.016 μs   (2.992 μs .. 3.050 μs)
aesonGetComplex      3.852 μs   (3.608 μs .. 4.320 μs)
```

In the simple get case, which reads `{a}`, we are a little slower than a hand coded parser `withObject "" (.:"a")`. The bottleneck seems to be in pattern matching, so one [small addition](https://github.com/libscott/aeson-quick/blob/a176c9baebcb0acd9416b3095cc1f751477cba2c/Data/Aeson/Quick.hs#L113) brought it down from >160ns to ~100ns. I looked at the GHC simplifier output and `Val` was [at the bottom of the case list](https://github.com/libscott/aeson-quick/blob/a176c9baebcb0acd9416b3095cc1f751477cba2c/Data/Aeson/Quick.hs#L112), which means that it going through something like: `if .. then .. else if .. then .. else if .. then .. else if .. then`. That takes ~60ns, apparently.

In [the complex get case](https://github.com/libscott/aeson-quick/blob/a176c9baebcb0acd9416b3095cc1f751477cba2c/test/Benchmark.hs#L43), somehow `aeson-quick` is faster than a hand coded parser? I have no idea. 

```
parseSimple          501.5 ns   (483.1 ns .. 517.3 ns)
parseComplex         2.459 μs   (2.421 μs .. 2.510 μs)
```
Parsing `Structure` values from the structure specification isn't free, but it only happens once. `Structure` implements `IsString` so it can be specified using a string literal, if you provide `{-# LANGUAGE OverloadedStrings #-}` at the top of your module.

```
quickSetSimple       266.1 ns   (261.7 ns .. 270.8 ns)
aesonSetSimple       263.4 ns   (261.2 ns .. 266.0 ns)
quickSetComplex      1.309 μs   (1.298 μs .. 1.325 μs)
aesonSetComplex      1.329 μs   (1.316 μs .. 1.344 μs)
```
In all of the set cases, `aeson-quick` seems near as dammit the same as hand crafted `aeson` values. I do not have an explanation. Anyhow, this benchmarking isn't particularly thorough. There's still lots to do!

## Future work

* Make it work better with Lens. Traversible would be nice, so it works correctly with `^?`.
* QuickCheck testing.
* More flexible (accept whitespace) and robust structure parsing.
* More tests.
* More thorough benchmarking.
* Syntax to delete data in a `Value`.
* More attention paid to edge cases in `Value` create / update.
* Way to make JSON parsing into custom data types more succinct and easy.

## Finally

Thanks to Bos for providing not one but **four** of the awesome libraries on which aeson-quick is built. That'd be Aeson, Criterion, Attoparsec, and Text. Amazing work, stunning really!
