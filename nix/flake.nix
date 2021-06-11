{
  description = "NixOS system configurations for various hosts";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: {

    nixosConfigurations = {
      "emma" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./machines/desktop.nix ./hardware/desktop.nix ];
      };

      # "daisy" = nixpkgs.lib.nixosSystem {
      #   system = "x86_64-linux";
      #   modules = [ ./machines/laptop.nix ];
      # };
    };
  };
}
