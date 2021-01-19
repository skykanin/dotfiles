{ config, pkgs, ... }:

{
  environment.pathsToLink = [
    "/share/nix-direnv"
  ];

  fonts = {
    enableDefaultFonts = true;

    fonts = with pkgs; [
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
    firewall.enable = true;
    firewall.allowedTCPPorts = [ 80 8080 3333 9630 44330 19000 19001 19002 19003];
    firewall.allowedUDPPorts = [ 80 9630 44330 19000 19001 19002 19003 ];
    networkmanager.enable = true;
  };
  
  nix = {
    autoOptimiseStore = true;
    binaryCaches = [ "https://hydra.iohk.io" ];
    binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    extraOptions = ''
       experimental-features = nix-command flakes
       keep-outputs = true
       keep-derivations = true
    '';
    gc.automatic = true;
    maxJobs = 2;
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
