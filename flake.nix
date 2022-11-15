{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay.${system} ];
        };
      in {
        overlay = self: super: {
          ocamlPackages = super.ocamlPackages.overrideScope' (self: super: {
            combinat = super.buildDunePackage {
              pname = "combinat";
              version = "3.0";
              duneVersion = "3";
              minimalOCamlVersion = "4.08";
              src = ./.;
            };
          });
        };
        defaultPackage = pkgs.ocamlPackages.combinat;
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.ocamlformat
            pkgs.opam
            pkgs.ocamlPackages.ocaml-lsp
            pkgs.ocamlPackages.core
            pkgs.ocamlPackages.core_unix
            pkgs.ocamlPackages.core_bench
            pkgs.ocamlPackages.ppx_jane
            pkgs.ocamlPackages.expect_test_helpers_core
          ];
          inputsFrom = [ pkgs.ocamlPackages.combinat ];
        };
      });
}
