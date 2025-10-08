{
  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/*";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";

    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        opam-repository.follows = "opam-repository";
      };
    };
  };
  outputs =
    {
      flake-utils,
      opam-nix,
      nixpkgs,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        on = opam-nix.lib.${system};
        src = ./.;
        localPackages = [ "oenv" ];

        devPackagesQuery = {
          ocaml-lsp-server = "*";
          utop = "*";
        };

        scope =
          let
            localPackagesQuery =
              with builtins;
              listToAttrs (
                map (p: {
                  name = p;
                  value = "*";
                }) localPackages
              );
            query = {
              ocaml-system = "*";
              ocamlformat = "*";
            }
            // devPackagesQuery
            // localPackagesQuery;
          in
          on.buildDuneProject {
            inherit pkgs;
            resolveArgs = {
              with-test = true;
              with-doc = true;
            };
          } "oenv" src query;

        devPackages = with builtins; attrValues (pkgs.lib.getAttrs (attrNames devPackagesQuery) scope);
        formatter = pkgs.nixfmt-rfc-style;

        devShells = rec {
          ci = pkgs.mkShell {
            inputsFrom = builtins.map (p: scope.${p}) localPackages;
            packages = [
              formatter
              scope.ocamlformat
              pkgs.actionlint
            ];
          };
          default = pkgs.mkShell {
            inputsFrom = [ ci ];
            buildInputs = devPackages ++ [ pkgs.nil ];
          };
          release = pkgs.mkShell {
            packaes = [
              pkgs.ocaml-ng.ocamlPackages.dune-release
              pkgs.ocaml-ng.ocamlPackages.dune
            ];
          };
        };
      in
      {
        legacyPackages = pkgs;
        packages =
          with builtins;
          listToAttrs (
            map (p: {
              name = p;
              value = scope.${p};
            }) localPackages
          );
        inherit devShells;
        inherit formatter;
      }
    );
}
