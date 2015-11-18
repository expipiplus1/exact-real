exact-real
==========

Exact real arithmetic implemented by fast binary Cauchy sequences.


Motivating Example
-------------------

It's well known that floating point arithmetic is the most awful thing.

Compare evaluating Eulers identity with a `Float`:

``` haskell
λ> let i = 0 :+ 1
λ> exp (i * pi) + 1 :: Complex Float
0.0 :+ (-8.742278e-8)
```

... and with a `CReal`

``` haskell
λ> import Data.CReal
λ> let i = 0 :+ 1
λ> exp (i * pi) + 1 :: Complex (CReal 0)
0 :+ 0
```

Current Status
--------------

The library is mostly feature complete. Performance is the next thing on the
todo list.


Contributing
------------

Contributions and bug reports are welcome!

Please feel free to contact me on GitHub or as "jophish" on freenode.

-Joe
