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
  networking.hostName = "emma";
  # Set your time zone.
  time.timeZone = "Europe/Oslo";

  environment.extraInit =
    ''
      xrandr --output DP-2 --mode 1920x1080 --rate 144 --left-of HDMI-0 --dpi 100
    '';

  services = {

    plex = {
      enable = true;
      openFirewall = true;
    };
    
    xserver = {
      exportConfiguration = true;
      videoDrivers = [ "nvidia" ];
      wacom.enable = true;
      #screenSection =
      #  ''
      #    Option         "metamodes" "DP-2: 1920x1080_144 +0+240 {ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}, HDMI-0: 1920x1080 +1920+0 { ForceCompositionPipeline=On, ForceFullCompositionPipeline=On, Rotation=90 }"
      #  '';
       xrandrHeads = [
         {
           output = "DP-2";
           primary = true;
         }
         {
           output = "HDMI-0";
           monitorConfig =
             ''
               Option "Rotate" "left"
             '';
         }
       ];
    };
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

}
