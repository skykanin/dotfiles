# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  options,
  pkgs,
  ...
}: let
  authorizedSshKeys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC4zVrpCIEQcPGiDWusYCBfv+Q9yGvFxaATSinxUYJpRVxqe78/aBG++hk3xbOVdeJF9NQWBgEMLC482pBLqIRwqG48+uy3s9FqUkoFCGqvqqD6ZNrHa3rdk03GEUpKGyUEYPZlJ8Y+t3HqxJAw+5SihNDj7PFGTnTC0hSiLGMqCVknc37Qt9dOo4iY7ANoDjERFpSjMPR3804Higqt+bhkblZOv52yTXnS8GHapZBJYjOCQHnbOJmmjbGZle/lRulYaEHFIWJGbJD7EzjwFUB/Z0h2qEtq2egq3jeFI4GiXrHao7o3pvzgGRt0WL3rBTm1ogXA1h77Oqs9jMfgFqVNYHetPAac/dtwmZY8rRv1zAcEwVqytjrxKQnR5Ghlt4hJoo3btauyNLld+vsDbNiAsru7iyHo9R40Rn5Wx616Ca6Qsf8fZsfVDfPRqLpoF/0kkF5VT9UHsUK4Hm/pBD92dZ79szm06k1B5DBpRgKzcwA9e29uj5xzHWo1Mk7qWgc= skykanin@emma"
  ];
in {
  imports = [
    ../modules/user.nix
    ../modules/nix.nix
    ../modules/emacs.nix
    ../modules/general/default.nix
    ../modules/general/nixos.nix
    ../modules/networking.nix
    ../modules/programs/default.nix
    ../modules/programs/nixos.nix
  ];

  # Local modules
  local = {
    user.authorizedSshKeys = authorizedSshKeys;
  };

  users.users.root.openssh.authorizedKeys.keys = authorizedSshKeys;

  services.openssh.enable = true;

  boot.tmp.cleanOnBoot = true;

  networking = {
    hostName = "dandy";
    domain = "";
  };

  zramSwap.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Oslo";

  # System packages
  environment.systemPackages = with pkgs; [
    bat
    babashka
    bottom
    curl
    direnv
    docker
    fd
    fish
    git
    kitty.terminfo
    neofetch
    nnn
    python3
    vim
    wget
  ];

  # Docker virtualisation security
  security.polkit.enable = true;

  # NixOS version
  system.stateVersion = "22.11";
}
