# Licensed under the Apache License, Version 2.0 or the MIT License.
# SPDX-License-Identifier: Apache-2.0 OR MIT
# Copyright Tock Contributors 2026.

{
  description = "Tock Integration Test Harness Nix Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, fenix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        inherit (pkgs) lib stdenv;

        tockloader = import (pkgs.fetchFromGitHub {
          owner = "tock";
          repo = "tockloader";
          rev = "a865b6f93f63b2af377546e8ad5c85b0a1fd53d1";
          sha256 = "sha256-bgA86FBa/va0kAXe4hn3wgBSidHY2GPmjYoatNmJd7I=";
        }) {
          inherit pkgs;
          withUnfreePkgs = false;
        };

        elf2tab = pkgs.rustPlatform.buildRustPackage rec {
          pname = "elf2tab";
          version = "0.13.0";

          src = pkgs.fetchFromGitHub {
            owner = "tock";
            repo = "elf2tab";
            rev = "v${version}";
            sha256 = "sha256-tVxYdokrHzlA8SqZN/+kpznvnnjx7X8z2Du4p4nU0V4=";
          };

          cargoHash = "sha256-5bQVk77gMtMfnRXf7rlRf/ABr73K9ZosB6E/gGZFpz4=";
        };

        # Pinned channel manifest hash, needs to be bumped on update.
        rustManifestSha256 = "sha256-wLCfThKHaLLW6rJwUGJaHOl0+LOcB4cTcDaM2C+/hhc=";

        rustBuild = fenix.packages.${system}.fromToolchainFile {
          file = ../../rust-toolchain.toml;
          sha256 = rustManifestSha256;
        };
      in
      {
        devShells.default = pkgs.mkShell {
          name = "tock-harness-dev";

          buildInputs = with pkgs; [
            # --- Rust toolchain ---
            rustBuild

            # --- tock-harness dependencies ---
            # (`serialport` crate links against libudev on Linux)
            pkg-config
            udev

            # --- Tock kernel toolchain ---
            llvm
            openocd

            # --- libtock-c app toolchain ---
            elf2tab
            gcc-arm-embedded
            pkgsCross.riscv32-embedded.buildPackages.gcc
            unzip

            # --- Flashing / running ---
            tockloader
            qemu
          ];

          LD_LIBRARY_PATH = lib.makeLibraryPath [
            stdenv.cc.cc.lib
            pkgs.libusb1
          ];

          # Instruct Tock build system to not use rustup:
          NO_RUSTUP = "1";

          # Nix defaults for OBJCOPY / OBJDUMP are wrong and don't work for
          # cross platforms, unset.
          shellHook = ''
            unset OBJCOPY
            unset OBJDUMP
          '';
        };
      });
}
