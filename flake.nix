{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPkgs = pkgs.ocaml-ng.ocamlPackages_4_14;
        defaultPackage = ocamlPkgs.buildDunePackage rec {
          pname = "combinat";
          version = "3.0";
          useDune3 = true;
          minimalOCamlVersion = "4.08";
          propagatedBuildInputs = [ ocamlPkgs.core ];
          checkInputs = [ ocamlPkgs.core_unix ocamlPkgs.core_bench ];
          src = ./.;
        };
      in {
        defaultPackage = defaultPackage;
        devShell = pkgs.mkShell {
          nativeBuildInputs =
            [ pkgs.ocamlformat pkgs.opam pkgs.ocamlPackages.ocaml-lsp ];
          inputsFrom = [ defaultPackage ];
        };
      });
}
