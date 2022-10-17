{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPkgs = pkgs.ocaml-ng.ocamlPackages;
        checkInputs = [
          ocamlPkgs.core
          ocamlPkgs.core_unix
          ocamlPkgs.core_bench
          ocamlPkgs.ppx_jane
          ocamlPkgs.expect_test_helpers_core
        ];
        defaultPackage = ocamlPkgs.buildDunePackage rec {
          pname = "combinat";
          version = "3.0";
          useDune3 = true;
          minimalOCamlVersion = "4.08";
          checkInputs = checkInputs;
          src = ./.;
        };
      in {
        defaultPackage = defaultPackage;
        devShell = pkgs.mkShell {
          nativeBuildInputs =
            [ pkgs.ocamlformat pkgs.opam pkgs.ocamlPackages.ocaml-lsp ]
            ++ checkInputs;
          inputsFrom = [ defaultPackage ];
        };
      });
}
