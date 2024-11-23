{
  description = "NixOS system configurations for various hosts";

  inputs = {
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts.url = "github:hercules-ci/flake-parts";

    # cppnix fork
    lix-module = {
      url = "git+https://git.lix.systems/lix-project/nixos-module?ref=refs/tags/2.91.1-1";
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
      systems = ["aarch64-darwin" "x86_64-darwin" "x86_64-linux"];

      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
      ];

      flake = {
        darwinConfigurations = {
          "nvjs-MacBook-Air" = inputs.nix-darwin.lib.darwinSystem rec {
            system = "aarch64-darwin";
            pkgs = inputs.self.legacyPackages.${system};
            specialArgs = {inherit inputs;};
            modules = [
              ./machines/macbook.nix
            ];
          };
          # MacOS VM
          "skykanins-iMac-Pro" = inputs.nix-darwin.lib.darwinSystem rec {
            system = "x86_64-darwin";
            pkgs = inputs.self.legacyPackages.${system};
            specialArgs = {inherit inputs;};
            modules = [
              ./machines/mac-vm.nix
            ];
          };
        };

        nixosConfigurations = {
          # Desktop
          emma = inputs.nixpkgs.lib.nixosSystem rec {
            system = "x86_64-linux";
            pkgs = inputs.self.legacyPackages.${system};
            specialArgs = {inherit inputs;};
            modules = [
              ./machines/desktop.nix
              ./hardware/desktop.nix
            ];
          };

          # Laptop
          daisy = inputs.nixpkgs.lib.nixosSystem rec {
            system = "x86_64-linux";
            pkgs = inputs.self.legacyPackages.${system};
            specialArgs = {inherit inputs;};
            modules = [
              ./machines/laptop.nix
              ./hardware/laptop.nix
              inputs.disko.nixosModules.disko
            ];
          };

          # Server
          dandy = inputs.nixpkgs.lib.nixosSystem rec {
            system = "x86_64-linux";
            pkgs = inputs.self.legacyPackages.${system};
            specialArgs = {inherit inputs;};
            modules = [
              ./machines/server.nix
              ./hardware/server.nix
            ];
          };
        };
      };

      perSystem = {
        pkgs,
        self',
        system,
        lib,
        ...
      }: {
        devShells.default = pkgs.mkShell {
          name = "nixd-shell";
          packages = [pkgs.nixd self'.formatter];
        };

        formatter = pkgs.alejandra;

        overlayAttrs = {};

        legacyPackages = pkgs;

        _module.args.pkgs = import inputs.nixpkgs {
          config.allowUnfree = true;
          config.permittedInsecurePackages = [ "olm-3.2.16" ];
          hostPlatform = system;
          inherit system;
          overlays = let
            compose = f: g: x: f (g x);
            mkCustomPackage = pair: _final: prev: {
              "${pair.name}" = prev.callPackage (pair.package) {};
            };
            listNixFilesRecursive = path:
              builtins.filter (lib.hasSuffix ".nix") (map toString (lib.filesystem.listFilesRecursive path));
          in
            map import (listNixFilesRecursive ./overlays)
            ++ map (compose mkCustomPackage import) (listNixFilesRecursive ./packages)
            ++ lib.optionals (lib.lists.elem system ["aarch64-darwin" "x86_64-darwin"]) [inputs.nixpkgs-firefox-darwin.overlay]
            ++ [ inputs.lix-module.overlays.default ];
        };
      };
    };
}
