{ compiler ? "ghc865" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              envy = haskellPackagesNew.callPackage ./nix/envy.nix {}; # old version (< 2) depended on by envy
              hal = pkgs.haskell.lib.markUnbroken haskellPackagesOld.hal;
              telegram-api = haskellPackagesNew.callPackage ./nix/haskell-telegram-api.nix {}; # patched version from git repo
              halma = haskellPackagesNew.callPackage ./halma/halma.nix {};
              halma-telegram-bot = haskellPackagesNew.callPackage ./halma-telegram-bot/halma-telegram-bot.nix {};
              servant-server = pkgs.haskell.lib.dontCheck haskellPackagesOld.servant-server; # tests fail to compile
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { halma-telegram-bot = pkgs.haskell.packages.${compiler}.halma-telegram-bot;
  }