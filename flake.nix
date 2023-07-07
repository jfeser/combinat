{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = final: prev: {
          ocamlPackages = prev.ocamlPackages.overrideScope' (ofinal: oprev: {
            combinat = ofinal.buildDunePackage {
              pname = "combinat";
              version = "3.0";
              duneVersion = "3";
              src = ./.;
            };
          });
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };

      in {
        overlays.default = overlay;
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
          inputsFrom = [ self.defaultPackage.${system} ];
        };
      });
}
