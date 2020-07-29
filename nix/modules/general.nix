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
      extraPackages = with pkgs; [
        vaapiVdpau
        libvdpau-va-gl
        intel-media-driver
      ];
    };
    pulseaudio.support32Bit = true;
  };
  
  networking.networkmanager.enable = true;
  
  nix = {
    autoOptimiseStore = true;
    package = pkgs.nixFlakes;
    extraOptions = pkgs.lib.optionalString (config.nix.package == pkgs.nixFlakes)
     "experimental-features = nix-command flakes";
  };

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
