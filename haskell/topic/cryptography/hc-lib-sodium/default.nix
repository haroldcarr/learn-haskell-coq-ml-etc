{ compiler ? "ghc902" }:

let
  project-name      = "hc-lib-sodium";
  sources           = import ./nix/sources.nix;
  pkgs              = import sources.nixpkgs {};
  gitignore         = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      project-name =
        hself.callCabal2nix
          project-name
          (gitignore ./.)
          {};
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.project-name
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
  exe    = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages.project-name);
  docker = pkgs.dockerTools.buildImage {
    name = project-name;
    config.Cmd = [ "${exe}/bin/${project-name}" ];
  };
in
{
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  project-name = myHaskellPackages.project-name;
}
