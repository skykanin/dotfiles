{
  description = "NixOS system configurations for various hosts";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    flake-utils.url = "github:numtide/flake-utils";

    git-ignore-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:IvanMalison/gitignore.nix";
    };

    idris2-main = {
      url =
        "github:idris-lang/idris2?rev=a4b99bd81c861a17f9573e92c5a9b6e8b72e3352";
      flake = false;
    };

    lsp-pinned = {
      url =
        "github:idris-community/idris2-lsp?rev=630e84ed4a7800fd3e70214e44e4a3be96b73ae9";
      flake = false;
    };

    idris2-pkgs = {
      url =
        "github:claymager/idris2-pkgs?rev=ac33a49d4d4bd2b50fddb040cd889733a02c8f09";
      # Uses newer dependency versions than in the flake
      inputs.lsp.follows = "lsp-pinned";
      inputs.idris2.follows = "idris2-main";
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
