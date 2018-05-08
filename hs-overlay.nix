{ pkgs }:

let
di-src = builtins.fetchGit {
  url = "https://github.com/k0001/di";
  rev = "2ceac4a8bcbce13bf02c5b3dd253f48eed2a543c";
};

in
self: super:
import "${di-src}/hs-overlay.nix" self super // {
  umzug = super.callPackage ./umzug/pkg.nix {};
  pipes-group = pkgs.haskell.lib.doJailbreak super.pipes-group;
  free = pkgs.haskell.lib.doJailbreak super.free;
}
