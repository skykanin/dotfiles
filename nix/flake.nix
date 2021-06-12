{
  description = "NixOS system configurations for various hosts";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: {

    nixosConfigurations = let
      systemRev = { ... }: {
        # Let 'nixos-version --json' know about the Git revision of this flake.
        system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
      };
    in {
      "emma" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ (systemRev) ./machines/desktop.nix ./hardware/desktop.nix ];
      };

      "daisy" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ (systemRev) ./machines/laptop.nix ./hardware/laptop.nix ];
      };
    };
  };
}
