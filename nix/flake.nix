{
  description = "NixOS system configurations for various hosts";

  inputs = {
    git-ignore-nix = {
      url = "github:IvanMalison/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nh = {
      url = "github:viperML/nh";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    kolide-config.url = "github:skykanin/kolide-launcher";
    xdg-desktop-portal-hyprland.url = "github:hyprwm/xdg-desktop-portal-hyprland";
  };

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: let
    overlays = {
      alejandra-remove-ads = import ./overlays/alejandra/default.nix;
    };
    pkgs =
      builtins.foldl' (acc: overlay: acc.extend overlay)
      nixpkgs.legacyPackages.x86_64-linux (builtins.attrValues overlays);
  in {
    devShells.x86_64-linux.default = pkgs.mkShell {
      name = "nixd-shell";
      buildInputs = with pkgs; [nixd];
    };
    formatter.x86_64-linux = pkgs.alejandra;

    nixosConfigurations = let
      inputs' = system:
        inputs
        // {
          nh = inputs.nh.packages.${system}.default;
          # Use xdg-desktop-portal-hyprland version 1.0
          # TODO: Remove this when it's bumped in nixpkgs
          xdg-desktop-portal-hyprland = inputs.xdg-desktop-portal-hyprland.packages.${system}.default;
        };
      lib = nixpkgs.lib;
      system-rev = {...}: {
        # Let 'nixos-version --json' know about the Git revision of this flake.
        system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
      };
      special-module = system: {_module.args = inputs' system;};
      kolide-module = inputs.kolide-config.nixosModules;
    in {
      # Desktop
      "emma" = lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = lib.lists.flatten [
          system-rev
          (special-module system)
          ./machines/desktop.nix
          ./hardware/desktop.nix
        ];
      };

      # Laptop
      "daisy" = lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = lib.lists.flatten [
          system-rev
          (special-module system)
          ./machines/laptop.nix
          ./hardware/laptop.nix
        ];
      };

      # Work laptop
      "iris" = lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = lib.lists.flatten [
          system-rev
          (special-module system)
          ./machines/work-laptop.nix
          ./hardware/work-laptop.nix
          kolide-module.${system}.default
        ];
      };

      # Server
      "dandy" = lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = lib.lists.flatten [
          system-rev
          (special-module system)
          ./machines/server.nix
          ./hardware/server.nix
        ];
      };
    };
  };
}
