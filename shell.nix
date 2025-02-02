{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, Chart, Chart-diagrams, hmatrix
      , hmatrix-gsl, lib, tasty, tasty-hunit, vector
      }:
      mkDerivation {
        pname = "happrox";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base hmatrix hmatrix-gsl vector ];
        executableHaskellDepends = [ base Chart Chart-diagrams vector ];
        testHaskellDepends = [ base tasty tasty-hunit ];
        license = lib.licenses.mit;
        mainProgram = "happrox";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

  # Additional developer tools
  devTools = with pkgs.haskellPackages; [
    hlint
    ormolu
    haskell-language-server
  ];

in

  if pkgs.lib.inNixShell then drv.env.overrideAttrs (oldAttrs: {
    buildInputs = (oldAttrs.buildInputs or []) ++ devTools;
  }) else drv
