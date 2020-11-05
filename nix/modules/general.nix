{ config, pkgs, ... }:

{
  environment.pathsToLink = [
    "/share/nix-direnv"
  ];

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
  
  networking = {
    firewall.allowedTCPPorts = [ 80 8080 3333 ];
    networkmanager.enable = true;
  };
  
  nix = {
    autoOptimiseStore = true;
    extraOptions = ''
       experimental-features = nix-command flakes
       keep-outputs = true
       keep-derivations = true
    '';
    gc.automatic = true;
    package = pkgs.nixFlakes;
    trustedBinaryCaches = [ "https://hydra.iohk.io" ];
  };

  nixpkgs.config.allowUnfree = true;

  qt5 = {
    enable = true;
    platformTheme = "gtk2";
    style = "gtk2";
  };
  
  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = true;
  
  time = {
    hardwareClockInLocalTime = true;
    timeZone = "Europe/Oslo";
  };
  
  virtualisation.docker.enable = true;
}
