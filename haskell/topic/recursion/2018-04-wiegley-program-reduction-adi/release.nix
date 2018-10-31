let
  pkgs = import <nixpkgs> { };

in
  { project= pkgs.haskellPackages.callPackage ./default.nix { };
  }
