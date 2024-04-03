{
  description = "From Scratch";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # overlays = [(import rust-overlay)];
        overlays = [];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      with pkgs;
      {
        devShells.default = mkShell {
            buildInputs = [
                just

                # Rust compiler
                # ( rust-bin.selectLatestNightlyWith (toolchain: toolchain.default.override {
                #     extensions = ["rust-src" "llvm-tools-preview" "rust-analyzer"];
                # }) )
                rustup
                cargo-nextest

                llvmPackages_15.libllvm
                mold
                gcc

                libffi
                libxml2
                pkg-config
                openssl
            ];

            shellHook = ''
                rustup default nightly
                rustup component add rust-analyzer --toolchain nightly
                cargo install cargo-dylint dylint-link
            '';
        };
    }
    );
}
