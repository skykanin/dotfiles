{
  description = "NixOS system configurations for various hosts";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";

    kolide.url = "github:skykanin/kolide-launcher";

    nh = {
      url = "github:viperML/nh";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];

      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
      ];

      flake = {
        nixosConfigurations =
          # let inputs' = system:
          #   inputs
          #   // {
          #     nh = inputs.nh.packages.${system}.default;
          #   };
          # lib = inputs.nixpkgs.lib;
          # special-module = system: {_module.args = inputs' system;}; in
          # remember to do `modules = lib.lists.flatten [ (special-module system) ...]`
          {
            # Desktop
            emma = inputs.nixpkgs.lib.nixosSystem rec {
              system = "x86_64-linux";
              pkgs = inputs.self.legacyPackages.${system};
              modules = [
                ./machines/desktop.nix
                ./hardware/desktop.nix
              ];
            };

            # Laptop
            daisy = inputs.nixpkgs.lib.nixosSystem rec {
              system = "x86_64-linux";
              pkgs = inputs.self.legacyPackages.${system};
              modules = [
                ./machines/laptop.nix
                ./hardware/laptop.nix
              ];
            };

            # Work laptop
            iris = inputs.nixpkgs.lib.nixosSystem rec {
              system = "x86_64-linux";
              pkgs = inputs.self.legacyPackages.${system};
              modules = [
                ./machines/work-laptop.nix
                ./hardware/work-laptop.nix
                inputs.kolide.nixosModules.${system}.default
              ];
            };

            # Server
            dandy = inputs.nixpkgs.lib.nixosSystem rec {
              system = "x86_64-linux";
              pkgs = inputs.self.legacyPackages.${system};
              modules = [
                ./machines/server.nix
                ./hardware/server.nix
              ];
            };
          };
      };

      perSystem = {
        pkgs,
        system,
        ...
      }: {
        devShells.default = pkgs.mkShell {
          name = "nixd-shell";
          buildInputs = with pkgs; [nixd];
        };

        formatter = pkgs.alejandra;

        overlayAttrs = {
          nh = inputs.nh.packages.${system}.default;
        };

        legacyPackages = pkgs;

        _module.args.pkgs = import inputs.nixpkgs {
          config.allowUnfree = true;
          inherit system;
          overlays = [(import ./overlays/alejandra/default.nix)];
        };
      };
    };
}
