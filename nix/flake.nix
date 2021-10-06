{
  description = "NixOS system configurations for various hosts";

  inputs = {
    emacs-overlay.url =
      "github:nix-community/emacs-overlay?rev=d8c2ba8a7c905c3ff94ce19263c293e9ffecee11";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }@inputs: {

    nixosConfigurations = let
      args = inputs;
      systemRev = { ... }: {
        # Let 'nixos-version --json' know about the Git revision of this flake.
        system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
      };
      special-module = {
        _module.args = inputs;
      };
    in {
      "emma" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ (systemRev) special-module ./machines/desktop.nix ./hardware/desktop.nix ];
      };

      "daisy" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ (systemRev) special-module ./machines/laptop.nix ./hardware/laptop.nix ];
      };
    };
  };
}
