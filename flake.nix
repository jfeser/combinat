{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPkgs = pkgs.ocaml-ng.ocamlPackages;
        combinat = ocamlPkgs.buildDunePackage {
          pname = "combinat";
          version = "3.0";
          duneVersion = "3";
          src = ./.;
        };
      in {
        defaultPackage = combinat;
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
          inputsFrom = [ combinat ];
        };
      });
}
