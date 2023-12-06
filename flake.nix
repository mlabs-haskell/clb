{
  description = "clb";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.iohkNix.url = "github:input-output-hk/iohk-nix";
  inputs.CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  outputs = { self, nixpkgs, flake-utils, haskellNix, iohkNix, CHaP }:
    let
      supportedSystems = [
        "x86_64-linux"	"x86_64-darwin"	"aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [
          # iohkNix.overlays.crypto provide libsodium-vrf, libblst and libsecp256k1.
          iohkNix.overlays.crypto

          # configure haskell.nix to use iohk-nix crypto librairies.
          iohkNix.overlays.haskell-nix-crypto

          haskellNix.overlay
          (final: prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                evalSystem = system;
                inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in flake // {
        legacyPackages = pkgs;
      });

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
