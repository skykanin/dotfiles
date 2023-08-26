{
  description = "NixOS system configurations for various hosts";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    git-ignore-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:IvanMalison/gitignore.nix";
    };

    idris2-main.url =
      "github:idris-lang/idris2?rev=c2bcc14e00794b19a7fc7ecc600f5a79b849f031";

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    kolide-config.url = "github:skykanin/kolide-launcher";
  };

  outputs = { self, nixpkgs, ... }@inputs: {

    nixosConfigurations = let
      inputs' = system:
        inputs // {
          idris2-main = inputs.idris2-main.packages.${system};
        };
      lib = nixpkgs.lib;
      system-rev = { ... }: {
        # Let 'nixos-version --json' know about the Git revision of this flake.
        system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
      };
      special-module = system: { _module.args = (inputs' system); };
      kolide-module = inputs.kolide-config.nixosModules;
    in {
      "desktop-emma" = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = lib.lists.flatten [
          (system-rev)
          (special-module system)
          ./machines/desktop.nix
          ./hardware/desktop.nix
        ];
      };

      "laptop-daisy" = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = lib.lists.flatten [
          (system-rev)
          (special-module system)
          ./machines/laptop.nix
          ./hardware/laptop.nix
        ];
      };

      "work-laptop-iris" = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = lib.lists.flatten [
          (system-rev)
          (special-module system)
          ./machines/work-laptop.nix
          ./hardware/work-laptop.nix
          kolide-module.${system}.default
        ];
      };

      "server-dandy" = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = lib.lists.flatten [
          (system-rev)
          (special-module system)
          ./machines/server.nix
          ./hardware/server.nix
        ];
      };
    };
  };
}
