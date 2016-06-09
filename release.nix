/* Build instructions for the continuous integration system Hydra. */

{ nixpkgs ? import <nixpkgs> {}
, exact-real-src ? { outPath = ./.; revCount = 0; gitTag = "dirty"; }
, supportedPlatforms ? [ "x86_64-linux" ]
, supportedCompilers ? [ "ghc7103" "ghc801" /*"ghcHEAD"*/]
}:


let 

  genAttrs = (import <nixpkgs> { }).lib.genAttrs;

in {

  exact-real = genAttrs supportedCompilers (ghcVer: 
    genAttrs supportedPlatforms (system:
      let
        pkgs = import <nixpkgs> { inherit system; };
        haskellPackages = pkgs.lib.getAttrFromPath ["haskell" "packages" ghcVer] pkgs;
      in
        haskellPackages.callPackage ./. {}
    )
  );

}

