import ./nix/package.nix {
    packageName = "my-xmonad";

    root = ./.;

    ghcVersion = "ghc822";

    withHoogle = true;

    profiling = false;

    # shellDepends = pkgs: haskellPkgs: [];

    pinnedPkgsPath = ./nix/nixpkgs.json;

    overrides = pkgs: { };

    haskellOverrides = {

      skipTests = [
        "ghc-mod"
      ];

      jailbreak = [
        "cabal-helper"
        "ghc-mod"
        "taffybar"
      ];

      skipHaddock = [
        "cabal-helper"
        "ghc-mod"
      ];

      justStaticExecutables = [
        "ghc-mod"
        "brittany"
      ];

      path = ./nix/overrides/haskell;

      manual = haskellLib: self: super: { };

    };

    results = {
      pkgs = [ ];
      haskellPkgs = [ ];
    };

}
