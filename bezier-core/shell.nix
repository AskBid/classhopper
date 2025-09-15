{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc96
    pkgs.cabal-install
    pkgs.haskellPackages.haskell-language-server
    pkgs.haskellPackages.hlint
  ];
}
