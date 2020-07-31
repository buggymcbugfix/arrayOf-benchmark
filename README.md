# Benchmarking the new `smallArrayOf` primop

As part of my Summer of Haskell project I implemented a new GHC primop
`smallArrayOf#` which creates an array from any number of elements passed via a
(nested) unboxed tuple.

## Background

Consider this program fragment as an example of using the new primop:

~~~hs
let
  a1 :: SmallArray# Bool = smallArrayOf# (# True, False #)
  a2 :: SmallArray# Bool = smallArrayOf# (# (# #), (# True, (# False #) #) #)
  a3 :: SmallArray# Int = smallArrayOf# (# 1, 2, 3 #)
in
  ...
~~~

> **Side note:** if we leave out the type signatures, GHC is prone to reject these
bindings with the error _You can't mix polymorphic and unlifted bindings_.)

Here `a1` and `a2` compile to equal arrays because GHC flattens unboxed tuple
arguments to curried function arguments during the `unarise` STG->STG pass since
the code generation backends have no notion of unboxed tuples (and sums). Hence
both `a1` and `a2` eventually compile to essentially
`smallArrayOf# True False` while `a3` compiles to `smallArrayOf# 1 2 3`.

In other words, `smallArrayOf#` is a **variadic primop**—the first of its kind
in GHC. Its type signature is currently:

~~~hs
smallArrayOf# :: forall (as :: TYPE r) a. as -> SmallArray# a
~~~

In other words it is pretty much untyped and highly unsafe: it would for example
allow filling an array with `Bool`s and typing it at `SmallArray# Int`. Yikes!

Once I find out how I would at least like to restrict the type to the following:

~~~hs
smallArrayOf# :: forall (as :: TYPE ('TupleRep rs)) a. as -> SmallArray# a
~~~

This rejects anything but unboxed tuples as an argument to `smallArrayOf#`,
which seems like a reasonable restriction. With the less restrictive type above
we can write singleton arrays without having to wrap the argument in an unboxed
tuple constructor, but this seems confusing.

I haven't figured out how to do this yet as it really isn't straightforward
(something something, `primops.txt.pp`)...

Time for activating our type-level arsenal?

We could definitely do some type class or type family magic to enforce a safe
interface. However, many (most?) primops are unsafe anyway and it seems to be
better to relegate the job of exposing a safe(r) interface to a library, such as
`primitive` for several reasons:

1. I don't know what the best possible mechanism is and I think it is healthy to
   let library writers try out different ways.
2. There is no precedent for class constraints or type families in
   [`GHC.Exts`](https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Exts.html).

[MkSmallArray.hs](MkSmallArray.hs) provides an example of a possible Typed
Template Haskell interface for creating `SmallArray#`s from lists and it in fact
guarantees typesafe usage, however at the cost of having a `Lift` constraint on
the element type:

~~~hs
smallArrayOf :: Lift a => [a] -> Q (TExp (SmallArray# a))
~~~

With this we can easily write code for generating static lookup tables for
computations that we don't want to have to do at runtime.

~~~hs
let a :: SmallArray# Int = $$(smallArrayOf [expensive i | i <- [0..3]]) in ...
~~~

> **Side note:** again, the type signature is usually needed, or alternatively
an explicit type application to `smallArrayOf`.

This generates the following code (where `w`..`z` have been bound to the
result of `expensive 0`..`expensive 3`):

~~~hs
let a :: SmallArray# Int = smallArrayOf# (# w, x, y, z #)
~~~

Without our new primop, the best we can do is to precompute our data into a list
and generate code for an unrolled array initialization.

[MkSmallArrayOld.hs](MkSmallArrayOld.hs) does exactly this (don't look at the
code—I ended up writing the `Exp` AST manually as I ran into issues with quoting
and unquoting). For example we generate something along the lines of the
following code with Template Haskell:

~~~hs
let a :: SmallArray# Int
  = unbox (runST (ST (\ s -> case newSmallArray# 4# undefined s of
    (# s0, marr #) ->
      let s1 = writeSmallArray# marr 0# w s0 in
      let s2 = writeSmallArray# marr 1# x s1 in
      let s3 = writeSmallArray# marr 2# y s2 in
      let s4 = writeSmallArray# marr 3# z s3 in
      let (# s5, arr #) = unsafeFreezeSmallArray# marr s4
      in (# s5, MkBox arr #))))
~~~

Basically what this code does is to make a new `MutableSmallArray#`, setting all
the fields to `undefined` and then writing all the fields and finally freezing
the array to an immutable one. (GHC can't just give us junk memory because the
GC would go haywire.)

So we are doing at least twice as much work as we should! And sure enough the
benchmark results reflect this intuition...

## Benchmark Results

`bench.hs` benchmarks how long it takes to initialize an array of known elements
with the new primop and to read the last element (blue). We compare this to the
best (to our knowledge) previously available method which is unrolled array
initialization via Template Haskell (green) and dynamic initialization in a
(non-unrolled) loop (yellow).

![A visualisation of output.csv](arrayOf-benchmark.png)

These benchmark results validate that the new primop achieves its goal at
offering markedly improved performance (~2.5x) for array initialization operations
when the number of elements N is statically known (we benchmark for 1 <= N <= 500).


## Running the Benchmark

Assuming that `$HC` points to GHC built via the `wip/buggymcbugfix/arrayOf-primop` branch:

    $ cabal run -w$HC --allow-newer
