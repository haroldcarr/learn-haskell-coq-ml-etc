{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, haskell-src-exts, microlens
      , microlens-th, mtl, stdenv, universum, wl-pprint
      }:
      mkDerivation {
        pname = "wiegley-program-reduction";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base haskell-src-exts microlens microlens-th mtl universum
          wl-pprint
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
