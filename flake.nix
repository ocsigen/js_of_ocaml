{
  description = "melange-jest Nix Flake";

  inputs.nixpkgs.url = "github:nix-ocaml/nix-overlays";

  outputs = { self, nixpkgs }:
    let
      forAllSystems = f: nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (system:
        let
          pkgs = nixpkgs.legacyPackages.${system}.extend (self: super: {
            ocamlPackages = super.ocaml-ng.ocamlPackages_5_3.overrideScope (oself: osuper: {
              pp = osuper.pp.overrideAttrs (_: { doCheck = false; });
              sedlex = osuper.sedlex.overrideAttrs (_: {
                src = super.fetchFromGitHub {
                  owner = "ocaml-community";
                  repo = "sedlex";
                  rev = "4dbaf572ed52281140be924a2c16fa4f287d88f0";
                  hash = "sha256-pRZ/GRTpBIa8ZcKdmCo1pmAHR3tJrIfNqU4IreEhO7g=";
                };
              });
              ppxlib = osuper.ppxlib.overrideAttrs (_: {
                src = builtins.fetchurl {
                  url = "https://github.com/ocaml-ppx/ppxlib/releases/download/0.36.0/ppxlib-0.36.0.tbz";
                  sha256 = "0d54j19vi1khzmw0ffngs8xzjjq07n20q49h85hhhcf52k71pfjs";
                };
              });
              reactiveData = osuper.buildDunePackage {
                pname = "reactiveData";
                version = "0.3.1";
                src = builtins.fetchurl {
                  url = "https://github.com/ocsigen/reactiveData/releases/download/0.3.1/reactiveData-0.3.1.tbz";
                  sha256 = "0wyw08rwx8924p88qc8dmw4nmpbhpc7i98nq4zzxn3036h2glwxd";
                };
                propagatedBuildInputs = [ oself.react ];
              };
            });
          });
        in
        f pkgs);
    in
    {
      devShells = forAllSystems (pkgs:
        let
          mkShell = { buildInputs ? [ ] }: pkgs.mkShell {
            inputsFrom = [ pkgs.ocamlPackages.js_of_ocaml-compiler ];
            nativeBuildInputs = with pkgs; [
              yarn
              nodejs_latest
            ] ++ (with pkgs.ocamlPackages; [
              ocamlformat
              merlin
            ]);
            inherit buildInputs;
          };
        in
        {
          default = mkShell {
            buildInputs = with pkgs.ocamlPackages; [
              reactiveData
              lwt
              tyxml
              graphics
              num
              re
              ocaml-lsp
              ppx_expect
            ];
          };
          release = mkShell {
            buildInputs = with pkgs; [ cacert curl ocamlPackages.dune-release git ];
          };
        });
    };
}
