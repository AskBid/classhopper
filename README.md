# ClassHopper

A 3D geometry kernel and visualization system written in pure Haskell for high-precision computational modeling of NURBS surfaces and curves.

## Vision

ClassHopper aims to bridge the gap between general parametric modeling tools (Grasshopper, Dynamo) and Class A surfacing requirements. The goal is a system where designers can define intricate surface patterns that unfold across multiple NURBS patches while maintaining dimensional consistency between tiles/modules despite varying surfaces parameterizations and flow directions.

## Working:

* NURBS evaluation with arbitrary degree and parameterization
* IGES file (ISO) parsing and geometry reconstruction
* Interactive OpenGL rendering of complex surfaces

## In Progress:

* Trimming curves and surface-surface intersection algorithms
* Element selection via ray casting from viewport

# Architecture

```
classhopper/
├── classhopper-geometry/   # Pure NURBS/B-spline mathematics
├── classhopper-io/         # IGES parser and file translation
└── classhopper-render/     # OpenGL visualization layer
```

# How to Build

ClassHopper uses Nix for reproducible builds. You'll need [NixGL](https://github.com/nix-community/nixGL) to access your system's OpenGL drivers from within the Nix environment.

## Why NixGL?

Nix isolates your build environment, but OpenGL drivers must access native GPU hardware. NixGL bridges this gap by:
* Wrapping execution with proper OpenGL driver environment variables
* Loading correct GPU drivers (NVIDIA, Intel, AMD) transparently
* Ensuring valid OpenGL context creation

### Build Steps

```bash
# Clone NixGL (one-time setup)
cd ..
git clone https://github.com/nix-community/nixGL.git
cd nixGL
nix build .#nixGLIntel  # or nixGLNvidia for NVIDIA GPUs

# Build and run ClassHopper
cd ../classhopper/
nix-shell
cd render
../nixGL/result/bin/nixGLIntel cabal run
```

For NVIDIA GPUs: Replace `nixGLIntel` with `nixGLNvidia`
For AMD GPUs: Use `nixGLDefault`
