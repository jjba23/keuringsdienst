{
  description = "Keuringsdienst flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            keuringsdienstProject = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc96";
              shell.tools = {
                cabal = { };
                #hlint = {};
                #haskell-language-server = {};
              };
              shell.buildInputs = with pkgs; [
                gnumake
                haskell-language-server
                hlint
                cabal-install
                cachix
                ormolu
                nixfmt
                statix
                deadnix
              ];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.keuringsdienstProject.flake { };
      in flake // {
        legacyPackages = pkgs;

        packages.default = flake.packages."keuringsdienst:lib:keuringsdienst";
        packages.test =
          flake.packages."keuringsdienst:test:keuringsdienst-spec";
      });

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
