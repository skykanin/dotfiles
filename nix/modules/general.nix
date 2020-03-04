{ config, pkgs, ... }:

{
  fonts = {
    enableDefaultFonts = true;

    fonts = with pkgs; [
    fira-code
    fira-code-symbols
    nerdfonts
    ];
  };
  
  hardware = {
    opengl = {
      driSupport32Bit = true;
      enable = true;
    };
    pulseaudio.support32Bit = true;
  };
  
  networking.networkmanager.enable = true;
  nixpkgs.config.allowUnfree = true;

  qt5 = {
    enable = true;
    platformTheme = "gtk2";
    style = "gtk2";
  };
  
  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = true;
  
  time.timeZone = "Europe/Oslo";
  virtualisation.docker.enable = true;
}
