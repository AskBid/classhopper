{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc96
    pkgs.cabal-install
    pkgs.haskell.packages.ghc96.haskell-language-server
    pkgs.haskellPackages.hlint
    pkgs.SDL2
    pkgs.pkg-config
    pkgs.libGL
    pkgs.libGLU
  ];
}
