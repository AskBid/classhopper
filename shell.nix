{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc96
    pkgs.cabal-install
    pkgs.haskell.packages.ghc96.haskell-language-server
    pkgs.haskellPackages.hlint
    pkgs.haskellPackages.hoogle

    # Haskell OpenGL bindings
    pkgs.haskellPackages.OpenGL
    pkgs.haskellPackages.GLFW-b

    # System libs for GL + GLFW
    pkgs.pkg-config
    pkgs.glfw
    pkgs.libGL
    pkgs.libGLU
    pkgs.mesa

    # X11 deps (required for GLFW on Linux)
    pkgs.xorg.libX11
    pkgs.xorg.libXi
    pkgs.xorg.libXrandr
    pkgs.xorg.libXrender
    pkgs.xorg.libXcursor
    pkgs.xorg.libXinerama
    pkgs.xorg.libXxf86vm
  ];

  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.libGLU}/lib:$LD_LIBRARY_PATH
    export LD_LIBRARY_PATH=${pkgs.libglvnd}/lib:$LD_LIBRARY_PATH
  '';
}
