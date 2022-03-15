{
  description = "NixOS system configurations for various hosts";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    flake-utils.url = "github:numtide/flake-utils";

    git-ignore-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:IvanMalison/gitignore.nix";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  };

  outputs = { self, nixpkgs, ... }@inputs: {

    nixosConfigurations = let
      args = inputs;
      lib = nixpkgs.lib;
      custom-services = lib.filesystem.listFilesRecursive ./modules/services;
      system-rev = { ... }: {
        # Let 'nixos-version --json' know about the Git revision of this flake.
        system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
      };
      special-module = { _module.args = inputs; };
    in {
      "emma" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = lib.lists.flatten [
          (system-rev)
          special-module
          ./machines/desktop.nix
          ./hardware/desktop.nix
          custom-services
        ];
      };

      "daisy" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = lib.lists.flatten [
          (system-rev)
          special-module
          ./machines/laptop.nix
          ./hardware/laptop.nix
          custom-services
        ];
      };

      "iris" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = lib.lists.flatten [
          (system-rev)
          special-module
          ./machines/work-laptop.nix
          ./hardware/work-laptop.nix
          custom-services
        ];
      };

    };
  };
}
