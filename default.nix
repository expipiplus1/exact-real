{ mkDerivation, base, checkers, directory, doctest, filepath
, groups, integer-gmp, memoize, QuickCheck, random, stdenv, tasty
, tasty-hunit, tasty-quickcheck, tasty-th
}:
mkDerivation {
  pname = "exact-real";
  version = "0.12.1";
  src = ./.;
  libraryHaskellDepends = [ base integer-gmp memoize random ];
  testHaskellDepends = [
    base checkers directory doctest filepath groups QuickCheck random
    tasty tasty-hunit tasty-quickcheck tasty-th
  ];
  homepage = "http://github.com/expipiplus1/exact-real";
  description = "Exact real arithmetic";
  license = stdenv.lib.licenses.mit;
}
