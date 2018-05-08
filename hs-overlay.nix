{ pkgs }:

let
di-src = builtins.fetchGit {
  url = "https://github.com/k0001/di";
  rev = "5a5e96e91fe69b8b304dd8d959c9b98ad02e03ae";
};

in
self: super:
import "${di-src}/hs-overlay.nix" self super // {
  umzug = super.callPackage ./umzug/pkg.nix {};
  pipes-group = pkgs.haskell.lib.doJailbreak super.pipes-group;
  free = pkgs.haskell.lib.doJailbreak super.free;
}
