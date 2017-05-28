{ nixpkgsBootstrap ? <nixpkgs>
, nixpkgs ? (import nixpkgsBootstrap {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "b12aacc7c18fb3f29febc842aaa3d0c0e5622546"; # release-17.03
    sha256 = "1qklmrvfnlk1r6rqxpddllfkv3pihfms370qfsznm959i7hxcv2v"; }
}:

let
pkgs = import nixpkgs {};
di-src = pkgs.fetchFromGitHub {
  owner = "k0001";
  repo = "di";
  rev = "f93350f157e0b317f133842c3e9050d9c1461fb4";
  sha256 = "0dfx249x0c7py91bi5wb23qqpgqxkpn30a3n0skgz0y03slrxk5r";
};

hsPackageSetConfig = self: super: {
  di = self.callPackage "${di-src}/pkg.nix" {};
  ex-pool = pkgs.haskell.lib.doJailbreak super.ex-pool;
  umzug = self.callPackage (import ./pkg.nix) {};
};

ghc802 = pkgs.haskell.packages.ghc802.override {
  packageSetConfig = hsPackageSetConfig;
};

in { inherit (ghc802) umzug; }
