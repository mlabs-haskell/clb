{
  description = "CLB Cardano Emulator";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix \\[\\e[0;1m\\]CLB \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
    cores = "1";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";

    haskell-nix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs";

    CHaP.url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
    CHaP.flake = false;

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
  };

  outputs = inputs@{ flake-parts, nixpkgs, haskell-nix, iohk-nix, CHaP, hercules-ci-effects, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        # Hercules CI effects module used to deploy to GitHub Pages
        hercules-ci-effects.flakeModule
        ./nix/pre-commit.nix
      ];
      debug = true;
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];

      perSystem = { config, system, ... }:
        let
          pkgs =
            import haskell-nix.inputs.nixpkgs {
              inherit system;
              overlays = [
                haskell-nix.overlay
                iohk-nix.overlays.crypto
                iohk-nix.overlays.haskell-nix-crypto
              ];
              inherit (haskell-nix) config;
            };
          project = pkgs.haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = "ghc928";
            inputMap = {
              "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
            };
            shell = {
              withHoogle = true;
              withHaddock = true;
              exactDeps = false;
              # TODO: use `export CABAL_DIR=$(pwd)/.cabal`
              shellHook = config.pre-commit.installationScript;
              tools = {
                cabal = { };
                haskell-language-server = { };
                hlint = { version = "3.6.1"; };
                cabal-fmt = { };
                fourmolu = { version = "0.14.0.0"; };
                hspec-discover = { };
              };
            };
          };
          flake = project.flake { };
        in
        {
          inherit (flake) checks devShells packages;
        };

      # On CI, build only on available systems, to avoid errors about systems without agents.
      # Please use aarch64-linux and x86_64-darwin sparingly as they run on smaller hardware.
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
