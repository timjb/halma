{ mkDerivation, async, base, containers, data-default
, diagrams-cairo, diagrams-gtk, diagrams-lib, grid, gtk, HUnit, mtl
, mvc, pipes, QuickCheck, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2, timeit
, vector-space-points
}:
mkDerivation {
  pname = "halma";
  version = "0.1.0.1";
  sha256 = "1k12f3ag54qqnl391i5sw3lmlrsjn693yjkigv9ypaghan02gh9p";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    async base containers data-default diagrams-cairo diagrams-gtk
    diagrams-lib grid gtk mtl mvc pipes timeit vector-space-points
  ];
  testDepends = [
    base containers grid HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2
  ];
  homepage = "https://github.com/timjb/halma";
  description = "Library implementing Halma rules";
  license = stdenv.lib.licenses.mit;
}
