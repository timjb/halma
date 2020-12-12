{ mkDerivation, base, bytestring, containers, hspec, mtl
, QuickCheck, quickcheck-instances, stdenv, text, time
, transformers
}:
mkDerivation {
  pname = "envy";
  version = "1.5.1.0";
  sha256 = "2dcacbb9901603f44e8e933849b532ba7b56ee2d7feff3980f9c7b556c4041e4";
  libraryHaskellDepends = [
    base bytestring containers mtl text time transformers
  ];
  testHaskellDepends = [
    base bytestring hspec mtl QuickCheck quickcheck-instances text time
    transformers
  ];
  description = "An environmentally friendly way to deal with environment variables";
  license = stdenv.lib.licenses.bsd3;
}
