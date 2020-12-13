{ compiler ? "ghc865", sources ? import ./nix/sources.nix }:

let
  nonStaticPkgs =
    with
      { overlay = _: pkgs: { niv = import sources.niv {}; }; };
    import sources.nixpkgs { overlays = [ overlay ]; };

  staticPkgs = (import "${sources.static-haskell-nix}/survey/default.nix" { compiler = compiler; normalPkgs = nonStaticPkgs; }).pkgs;
  haskellLib = staticPkgs.haskell.lib;

  staticHaskellPkgs = staticPkgs.haskell.packages.${compiler}.override {
    overrides = haskellPackagesNew: haskellPackagesOld: rec {
      envy = haskellPackagesNew.callPackage ./nix/envy.nix {}; # old version (< 2) depended on by envy
      hal = haskellLib.markUnbroken haskellPackagesOld.hal;
      telegram-api = haskellPackagesNew.callPackage ./nix/haskell-telegram-api.nix {}; # patched version from git repo
      halma = haskellPackagesNew.callPackage ./halma/halma.nix {};
      halma-telegram-bot = haskellLib.overrideCabal
        (haskellLib.justStaticExecutables (haskellPackagesNew.callPackage ./halma-telegram-bot/halma-telegram-bot.nix {}))
        {
          configureFlags = [
            "--ghc-option=-optl=-static"
            "--extra-lib-dirs=${staticPkgs.gmp6.override { withStatic = true; }}/lib"
            "--extra-lib-dirs=${staticPkgs.zlib.static}/lib"
            "--extra-lib-dirs=${staticPkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
            #"--disable-executable-stripping"
          ];
        };
      servant-server = haskellLib.dontCheck haskellPackagesOld.servant-server; # tests fail to compile
    };
  };

in
  { halma-telegram-bot = staticHaskellPkgs.halma-telegram-bot;
  }