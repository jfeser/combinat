let
  overlay = self: super: {
    ocamlPackages = super.ocaml-ng.ocamlPackages_4_14.overrideScope' (self: super: {
      ocaml = super.ocaml.override { flambdaSupport = true; };
    });
  };
in

let pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/3bbb296d9a0088c314ce83038b896753bbe33acb.tar.gz")
  {
    overlays = [ overlay ];
  };
in

let deps = (with pkgs.ocamlPackages; [
      ocaml
      findlib
      dune_3
      core
      core_bench
      core_unix
      ppx_jane
      odoc
    ]);
    dev_deps = deps ++ [pkgs.ocamlformat pkgs.ocamlPackages.ocaml-lsp];
in

pkgs.mkShell {
  buildInputs = dev_deps;
}
