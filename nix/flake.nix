{
  description = "NixOS system configurations for various hosts";

  inputs = {
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts.url = "github:hercules-ci/flake-parts";

    kolide.url = "github:skykanin/kolide-launcher";

    nh = {
      url = "github:viperML/nh";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs-firefox-darwin.url = "github:bandithedoge/nixpkgs-firefox-darwin";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["aarch64-darwin" "x86_64-linux"];

      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
      ];

      flake = {
        darwinConfigurations."nvjs-MacBook-Air" = inputs.nix-darwin.lib.darwinSystem rec {
          system = "aarch64-darwin";
          pkgs = inputs.self.legacyPackages.${system}.extend (inputs.nixpkgs-firefox-darwin.overlay);
          modules = [
            ./machines/macbook.nix
            {_module.args = {inherit inputs;};}
          ];
        };
        nixosConfigurations = {
          # Desktop
          emma = inputs.nixpkgs.lib.nixosSystem rec {
            system = "x86_64-linux";
            pkgs = inputs.self.legacyPackages.${system};
            modules = [
              ./machines/desktop.nix
              ./hardware/desktop.nix
              {_module.args = {inherit inputs;};}
            ];
          };

          # Laptop
          daisy = inputs.nixpkgs.lib.nixosSystem rec {
            system = "x86_64-linux";
            pkgs = inputs.self.legacyPackages.${system};
            modules = [
              ./machines/laptop.nix
              ./hardware/laptop.nix
              inputs.disko.nixosModules.disko
              {_module.args = {inherit inputs;};}
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
              {_module.args = {inherit inputs;};}
            ];
          };

          # Server
          dandy = inputs.nixpkgs.lib.nixosSystem rec {
            system = "x86_64-linux";
            pkgs = inputs.self.legacyPackages.${system};
            modules = [
              ./machines/server.nix
              ./hardware/server.nix
              {_module.args = {inherit inputs;};}
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
          packages = with pkgs; [nixd];
        };

        formatter = pkgs.alejandra;

        overlayAttrs = {
          nh = inputs.nh.packages.${system}.default;
        };

        legacyPackages = pkgs;

        _module.args.pkgs = import inputs.nixpkgs {
          config.allowUnfree = true;
          hostPlatform = system;
          inherit system;
          overlays = [(import ./overlays/alejandra/default.nix)];
        };
      };
    };
}
