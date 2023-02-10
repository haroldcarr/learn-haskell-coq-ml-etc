{ pkgs, ... }:

{
  packages = [ pkgs.git
               pkgs.ghc
             ];

  languages.haskell.enable = true;
  languages.nix.enable     = true;
  languages.rust.enable    = true;
}
