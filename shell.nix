# { stdenv, pkgs, haskell }:
with import <nixpkgs> { };

stdenv.mkDerivation rec {
  name = "caps";
  buildInputs = [
    haskell.compiler.ghc902
    pkgs.cabal-install
    pkgs.pkgconfig
  ];
}
