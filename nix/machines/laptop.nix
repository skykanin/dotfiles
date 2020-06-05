{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ../modules/bluetooth.nix
      ../modules/boot-efi.nix
      ../modules/general.nix
      ../modules/packages.nix
      ../modules/printing.nix
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

  environment.variables = {
    MESA_LOADER_DRIVER_OVERRIDE = "iris";
  };
  hardware.opengl.package = (pkgs.mesa.override {
    galliumDrivers = [ "nouveau" "virgl" "swrast" "iris" ];
  }).drivers;
  programs.light.enable = true;
  services.compton.vSync = true;
  # Suspend on lid close
  services.logind.lidSwitch = "suspend";
  services.xserver.libinput.naturalScrolling = false;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?

}
