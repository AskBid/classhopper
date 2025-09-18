{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc96
    pkgs.cabal-install
    pkgs.haskell.packages.ghc96.haskell-language-server
    pkgs.haskellPackages.hlint
    pkgs.SDL2
    pkgs.pkg-config

    # OpenGL bits
    pkgs.libGL    # development files
    pkgs.libGLU   # GLU extension
    pkgs.mesa     # runtime libraries (libGL.so.1)
  ];
  
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.libGLU}/lib:$LD_LIBRARY_PATH
    export LD_LIBRARY_PATH=${pkgs.libglvnd}/lib:$LD_LIBRARY_PATH
  '';
}
