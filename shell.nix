# { stdenv, pkgs, haskell }:
with import <nixpkgs> { };

stdenv.mkDerivation rec {
  name = "caps";
  buildInputs = [
    haskell.compiler.ghc843
    pkgs.cabal-install
    pkgs.pkgconfig
  ];
}
