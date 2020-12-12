{ mkDerivation, aeson, base, containers, data-default, diagrams-lib
, grid, HUnit, QuickCheck, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2
}:
mkDerivation {
  pname = "halma";
  version = "0.3.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers data-default diagrams-lib grid
  ];
  testHaskellDepends = [
    base containers grid HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2
  ];
  homepage = "https://github.com/timjb/halma";
  description = "Library implementing Halma rules";
  license = stdenv.lib.licenses.mit;
}
