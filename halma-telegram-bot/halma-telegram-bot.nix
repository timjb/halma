{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-core
, amazonka-s3, base, bytestring, conduit-extra, containers
, data-default, diagrams-lib, diagrams-rasterific, directory
, exceptions, filepath, hal, halma, http-client, http-client-tls
, lens, megaparsec, mtl, optparse-applicative, resourcet
, semigroups, servant-client, stdenv, telegram-api, temporary, text
, timeit, transformers, vector
}:
mkDerivation {
  pname = "halma-telegram-bot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring containers data-default
    diagrams-lib diagrams-rasterific directory exceptions filepath
    halma http-client http-client-tls megaparsec mtl
    optparse-applicative semigroups servant-client telegram-api
    temporary text timeit transformers vector
  ];
  executableHaskellDepends = [
    aeson amazonka amazonka-core amazonka-s3 base bytestring
    conduit-extra hal http-client http-client-tls lens
    optparse-applicative resourcet telegram-api text
  ];
  homepage = "https://github.com/timjb/halma";
  description = "Telegram bot for playing Halma";
  license = stdenv.lib.licenses.mit;
}
