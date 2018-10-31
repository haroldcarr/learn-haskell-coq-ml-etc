{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, stdenv
                     , base
                     , bifunctors
                     , bytestring
                     , mtl
                     , network
                     , pipes
                     , pipes-bytestring
                     , pipes-http
                     , pipes-network
                     , pipes-safe
                     , transformers
      }:
      mkDerivation {
        pname = "pipes-playground";
        version = "0.1.0.0";
        src = ./src;
        libraryHaskellDepends = [
                       base
                       bifunctors
                       bytestring
                       mtl
                       network
                       pipes
                       pipes-bytestring
                       pipes-http
                       pipes-network
                       pipes-safe
                       transformers
        ];
        license = stdenv.lib.licenses.unfree;
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
