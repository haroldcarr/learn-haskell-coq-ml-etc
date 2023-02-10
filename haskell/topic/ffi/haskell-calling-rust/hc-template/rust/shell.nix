# based on https://www.adelbertc.com/emacs-rls-nix/

let
  rust-version = "1.64.0";

  nixpkgs = fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "227de2b3bbec142f912c09d5e8a1b4e778aa54fb";
  };

  mozilla-overlay =
    import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);

  pkgs = import nixpkgs {
    overlays = [ mozilla-overlay ];
  };

  rust-channel = pkgs.rustChannelOf {
    channel = rust-version;
  };

  rust = rust-channel.rust.override {
    extensions = [ "rust-src" "rust-analyzer-preview" ];
  };

  cargo = rust-channel.cargo;
in
  pkgs.mkShell {
    name        = "rust-dev";
    buildInputs = [ rust cargo pkgs.libiconv ];
  }
