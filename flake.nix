{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            adventOfCodeProject =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc925";
                evalSystem = "x86_64-linux";
                shell.tools = {
                  cabal = {};
                  hlint = {};
                  haskell-language-server = {};
                };
                shell.withHoogle = false;
                modules = [{
                    enableLibraryProfiling = true; 
                  }];
                ## Non-Haskell shell tools go here
                #shellFor.packages = with pkgs; [
                #  nixpkgs-fmt
                #];
                };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.adventOfCodeProject.flake {};
      in flake // {
        legacyPackages = pkgs;
        packages.default = flake.packages."aoc:exe:aoc";
      });

  # --- Flake Local Nix Configuration ----------------------------
  #nixConfig = {
  #  # This sets the flake to use the IOG nix cache.
  #  # Nix should ask for permission before using it,
  #  # but remove it here if you do not want it to.
  #  extra-substituters = ["https://cache.iog.io"];
  #  extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
  #  allow-import-from-derivation = "true";
  #};
}
