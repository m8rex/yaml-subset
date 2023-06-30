{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/1ad58fd33b129f13fe0e1af44ff0a8ff27210a76.tar.gz") {} }:


let
  rust_overlay = import (builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz");
  pkgs = import <nixpkgs> { overlays = [ rust_overlay ]; };
  rustVersion = "1.67.1";
  rust = pkgs.rust-bin.stable.${rustVersion}.default.override {
    extensions = [
      "rust-src" # for rust-analyzer
    ];
  };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    (ghc.withPackages (pkgs: [ pkgs.yaml pkgs.scientific pkgs.aeson pkgs.unordered-containers  ]))
    rust
    rustfmt
    clippy
    cargo-insta
    (vscode-with-extensions.override {
      vscode = vscodium;
      vscodeExtensions = with vscode-extensions; [
        bbenoist.nix
	arrterian.nix-env-selector
 	matklad.rust-analyzer
      ] ++ vscode-utils.extensionsFromVscodeMarketplace [
    ];
  })
  ];
}


