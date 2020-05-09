{ pkgs ? import <nixpkgs> { }, compiler ? null, hoogle ? true }:

let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

  compiler' = if compiler != null then
    compiler
  else
    "ghc" + pkgs.lib.concatStrings
    (pkgs.lib.splitVersion pkgs.haskellPackages.ghc.version);

  # Any overrides we require to the specified haskell package set
  haskellPackages = with pkgs.haskell.lib;
    pkgs.haskell.packages.${compiler'}.override {
      overrides = self: super:
        { } // pkgs.lib.optionalAttrs hoogle {
          ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
          ghcWithPackages = self.ghc.withPackages;
        };
    };

  # Generate a haskell derivation using the cabal2nix tool on `package.yaml`
  drv = let old = haskellPackages.callCabal2nix "" src { };
  in old // {
    env = pkgs.lib.overrideDerivation old.env (attrs:
      pkgs.lib.optionalAttrs hoogle {
        shellHook = attrs.shellHook + ''
          export HIE_HOOGLE_DATABASE="$(cat $(${pkgs.which}/bin/which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p')"
        '';
      });
  };

in if pkgs.lib.inNixShell then drv.env else drv
