{ packageName      ? null,
  root,
  ghcVersion,
  withHoogle       ? true,
  profiling        ? false,
  shellDepends     ? pkgs: haskellPkgs: with pkgs; with haskellPkgs; [
    # brittany
    cabal-helper
    cabal-install
    ghc-mod
    haskdogs
    hasktags
    hlint
    hpack
    # lushtags
    stylish-haskell
  ],
  pinnedPkgsPath   ? ./nixpkgs.json,
  overrides        ? pkgs: { },
  haskellOverrides ? {
    skipTests = [];
    jailbreak = [];
    skipHaddock = [];
    justStaticExecutables = [];
    path = ./overrides/haskell;
    manual = haskellLib: super: self: {};
  },
  results ?  {
    pkgs = [];
    haskellPkgs = [];
  }
} :

let

  name = if packageName == null
         then (builtins.baseNameOf root)
         else packageName;

  mkPkgs = path:
    let
      bootstrap = import <nixpkgs>;
    in
      if (builtins.pathExists path)
      then
        let
          pkgs = bootstrap { };
          json = builtins.fromJSON (builtins.readFile path);
          src = pkgs.fetchFromGitHub {
            owner = "NixOS";
            repo  = "nixpkgs";
            inherit (json) rev sha256;
          };
        in (import src)
      else bootstrap;

  hOverrides = {
    skipTests             = haskellOverrides.skipTests or [];
    jailbreak             = haskellOverrides.jailbreak or [];
    skipHaddock           = haskellOverrides.skipHaddock or [];
    justStaticExecutables = haskellOverrides.justStaticExecutables or [];
    path                  = haskellOverrides.path or ./overrides/haskell;
    manual                = haskellOverrides.manual or (_: _: _: {});
  };

  mkEnv = {
    allowUnfree ? true,
    withHoogle  ? false,
    profiling   ? false,
    shell       ? false
  } :
  let
    config = {
      allowUnfree = allowUnfree;

      packageOverrides = pkgs: rec {
        haskell = pkgs.haskell // {
          packages = pkgs.haskell.packages // {
            "${ghcVersion}" =
              let
                configOverrides = self: super: {
                  ghc =
                    if withHoogle
                    then super.ghc // { withPackages = super.ghc.withHoogle; }
                    else super.ghc;

                  mkDerivation = args: super.mkDerivation (args // {
                    enableLibraryProfiling = profiling;
                  });
                };

                rootOverride = self: super: {
                  "${name}" = self.callCabal2nix "${name}" root { };
                };

                generatedOverrides = self: super:
                  if (builtins.pathExists hOverrides.path)
                  then
                    let
                      toPackage = file: _: {
                        name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;
                        value = self.callPackage (hOverrides.path + "/${file}") { };
                      };
                    in pkgs.lib.mapAttrs' toPackage (builtins.readDir hOverrides.path)
                  else {};

                makeOverrides = modifier: names: self: super:
                  let
                    toPackage = name: {
                      inherit name;
                      value = modifier super.${name};
                    };
                  in builtins.listToAttrs (map toPackage names);

                manualOverrides = hOverrides.manual haskellLib;

                shellOverride = self: super:
                  if (shell)
                  then {
                    "${name}" =
                      haskellLib.overrideCabal super."${name}"
                      ( drv: {
                        doBenchmark = true;
                        buildDepends = shellDepends pkgs super;
                      });
                    }
                  else {};

                haskellLib = pkgs.haskell.lib;

              in with haskellLib;
                pkgs.haskell.packages."${ghcVersion}".override {
                  overrides = composeExtensions [
                    configOverrides
                    generatedOverrides
                    (makeOverrides dontCheck hOverrides.skipTests)
                    (makeOverrides doJailbreak hOverrides.jailbreak)
                    (makeOverrides dontHaddock hOverrides.skipHaddock)
                    (makeOverrides justStaticExecutables hOverrides.justStaticExecutables)
                    rootOverride
                    manualOverrides
                    shellOverride
                  ];
                };
          };
        };
      } // overrides pkgs;
    };

    pkgs = mkPkgs pinnedPkgsPath { inherit config; };

    lib = pkgs.lib;

    haskellPkgs = pkgs.haskell.packages."${ghcVersion}";

    haskellLib = pkgs.haskell.lib;

    resultSet =
      (toResultSet pkgs (results.pkgs or [])) //
      (toResultSet haskellPkgs ((results.haskellPkgs or []) ++ [ name ]));

    ### Utility functions

    /* enableExecutableProfiling modifies a haskell package to enable
       profiling of the executables in the build product.
    */
    enableExecutableProfiling = drv: haskellLib.overrideCabal drv (drv: { enableExecutableProfiling = true; });

    /* a composeExtensions version for working with a list of extensions
    */
    composeExtensions = pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});

    toResultSet = pkgs: names:
      lib.foldl (acc: name: acc // { "${name}" = pkgs."${name}"; }) {} names;

  in {

    inherit pkgs haskellPkgs resultSet;

    ghc = haskellPkgs.ghc;

  };

  shellEnv = mkEnv {
    withHoogle = withHoogle;
    profiling  = profiling;
    shell      = true;
  };

  releaseEnv = mkEnv {
    withHoogle = false;
    shell      = false;
  };

  inNixShell = shellEnv.pkgs.lib.inNixShell;

  shellDrv = shellEnv.resultSet."${name}".env.overrideAttrs ( oldAttrs: {
    shellHook = ''
      export NIX_GHC_LIBDIR="$(dirname $(type -p ghc))/../lib/ghc-${shellEnv.ghc.version}"
      export cabal_helper_libexecdir=$(dirname $(which ghc))
    '';
  } );

  releaseDrvs = releaseEnv.resultSet;

in
  if inNixShell
  then shellDrv
  else releaseDrvs
