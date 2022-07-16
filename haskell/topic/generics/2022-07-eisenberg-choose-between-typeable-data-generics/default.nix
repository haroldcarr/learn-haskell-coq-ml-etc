{ compiler ? "ghc921" }:  # "ghc8107"


let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "eisenberg-choose-between-typeable-data-generics" =
        hself.callCabal2nix
          "eisenberg-choose-between-typeable-data-generics"
          (gitignore ./.)
          {};
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."eisenberg-choose-between-typeable-data-generics"
    ];
    buildInputs = [
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.hpack
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = false;
  };
in
{
  inherit shell;
  inherit myHaskellPackages;
  "eisenberg-choose-between-typeable-data-generics" = myHaskellPackages."eisenberg-choose-between-typeable-data-generics";
}
