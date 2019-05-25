{ nixpkgs ? import <nixpkgs> {} }:

let pkg = nixpkgs.callPackage ./release.nix {};

in

nixpkgs.buildEnv {
  name = "shell";
  paths = [];
  buildInputs = with nixpkgs.haskellPackages; [
    ghcid
    stylish-haskell
    hasktags
    cabal-install
    (ghcWithHoogle (_: pkg.buildInputs ++ pkg.propagatedBuildInputs))
  ];
}
