{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc801"
}:

let

  f = import ./default.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
