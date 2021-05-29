{ nixpkgsSrc ? builtins.fetchTarball {
  url =
    "https://github.com/NixOS/nixpkgs/archive/540dccb2aeaffa9dc69bfdc41c55abd7ccc6baa3.tar.gz"; # nixos-unstable
  sha256 = "1j58m811w7xxjncf36hqcjqsfj979hkfcwx9wcrm3g3zbayavapg";
}, pkgs ? import nixpkgsSrc { }, compiler ? null, extraOverrides ? _: _: { }
, modifier ? x: x }:

let
  haskellPackages = if compiler == null then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage {
  name = "";
  root = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  overrides = with pkgs.haskell.lib;
    pkgs.lib.composeExtensions (_self: _super: { }) extraOverrides;
  inherit modifier;
}

