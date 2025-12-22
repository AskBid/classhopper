{ pkgs ? import <nixpkgs> {} }:

let
  haskell = pkgs.haskell.packages.ghc98;
in

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc98
    haskell.cabal-install

    # NOTE: intentionally NOT adding haskell.haskell-language-server here
    # because building HLS inside the shell is causing the ghc-exactprint
    # / TransformT type mismatch errors.

    # Useful Haskell tools (no HLS)
    haskell.hlint
    haskell.hoogle

    # Haskell libraries used by your project
    haskell.OpenGL
    haskell."GLFW-b"
    haskell.linear

    # System deps for OpenGL + GLFW
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
    export LD_LIBRARY_PATH=${pkgs.glfw}/lib:${pkgs.libGL}/lib:${pkgs.libGLU}/lib:${pkgs.mesa}/lib:${pkgs.xorg.libX11}/lib:${pkgs.xorg.libXi}/lib:${pkgs.xorg.libXrandr}/lib:${pkgs.xorg.libXrender}/lib:${pkgs.xorg.libXcursor}/lib:${pkgs.xorg.libXinerama}/lib:${pkgs.xorg.libXxf86vm}/lib:$LD_LIBRARY_PATH

    echo "Entered nix-shell: GHC $(ghc --version)"
    echo ""
    echo "To use Hoogle:"
    echo "  1. cabal haddock all"
    echo "  2. hoogle generate --local"
    echo "  3. hoogle server --local --port=8080"
  '';
}
