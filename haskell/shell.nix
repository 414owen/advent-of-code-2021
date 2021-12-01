let
  sources = import ./nix/sources.nix;
in

{ pkgs ? import sources.nixpkgs {}, compiler ? "ghc901", doBenchmark ? false }:

let
  f = import ./default.nix;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = variant (haskellPackages.callPackage f {});
in

  if pkgs.lib.inNixShell then drv.env else drv
