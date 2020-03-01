# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ../modules/bluetooth.nix
      ../modules/boot-efi.nix
      ../modules/general.nix
      ../modules/packages.nix
      ../modules/programs.nix
      ../modules/redshift.nix
      ../modules/sound.nix
      ../modules/ssh.nix
      ../modules/user.nix
      ../modules/xserver.nix
    ];

  # Define hostname
  networking.hostName = "daisy";
  # Set your time zone.
  time.timeZone = "Europe/Oslo";

  programs.light.enable = true;
  services.compton.vSync = true;
  # Suspend on lid close
  services.logind.lidSwitch = "suspend"; 

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

}
