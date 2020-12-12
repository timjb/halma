{ mkDerivation, aeson, ansi-wl-pprint, base, binary, bytestring
, containers, fetchgit, filepath, hjpath, hspec, http-api-data
, http-client, http-client-tls, http-media, http-types, mime-types
, mtl, optparse-applicative, random, servant, servant-client
, servant-client-core, stdenv, string-conversions, text
, transformers, utf8-string
}:
mkDerivation {
  pname = "telegram-api";
  version = "0.7.1.0";
  src = fetchgit {
    url = "https://github.com/klappvisor/haskell-telegram-api.git";
    sha256 = "0mzhigdyj5jdwycmz587s05zp5c7wcf7njw3x866iln59kp0rgi3";
    rev = "abbfd76c40f2783c113b660184a03cc94d58e751";
    fetchSubmodules = true;
  };
  patches = [ ./0001-relax-servant-client-version.patch ];
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base binary bytestring containers http-api-data http-client
    http-media http-types mime-types mtl servant servant-client
    servant-client-core string-conversions text transformers
  ];
  testHaskellDepends = [
    aeson ansi-wl-pprint base binary filepath hjpath hspec http-client
    http-client-tls http-types optparse-applicative random servant
    servant-client servant-client-core text transformers utf8-string
  ];
  homepage = "http://github.com/klappvisor/haskell-telegram-api#readme";
  description = "Telegram Bot API bindings";
  license = stdenv.lib.licenses.bsd3;
}
