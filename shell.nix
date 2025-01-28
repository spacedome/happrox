{ system ? builtins.currentSystem, devTools ? true }:
let
  pkgs = import <nixpkgs> {};
  myHaskellPackages = pkgs.haskellPackages;
in myHaskellPackages.shellFor {
  packages = p: [ ];
  nativeBuildInputs = with pkgs;
    [ gsl blas lapack ghc cabal-install ] ++ lib.optional devTools [
      hlint
      ormolu
      (ghc.withPackages (p: [ p.haskell-language-server p.hmatrix p.hmatrix-gsl ]))
    ];
}
