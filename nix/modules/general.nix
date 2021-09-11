{ config, pkgs, ... }:

{
  environment.pathsToLink = [
    "/share/nix-direnv"
  ];
  
  environment.variables.EDITOR = "vim";
  fonts = {
    fonts = with pkgs; [
      (nerdfonts.override { fonts = [
        "JetBrainsMono"
        "VictorMono"
      ];})
      noto-fonts
      unifont
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
    nameservers = ["9.9.9.11"];
    networkmanager.enable = true;
  };
  
  nix = {
    autoOptimiseStore = true;
    binaryCaches = [ "https://hydra.iohk.io" ];
    binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    extraOptions = ''
       experimental-features = nix-command flakes ca-references
       keep-outputs = false 
       keep-derivations = false 
    '';
    gc.automatic = true;
    maxJobs = 2;
    package = pkgs.nixFlakes;
    trustedBinaryCaches = [ "https://hydra.iohk.io" ];
    trustedUsers = [ "root" "skykanin" ];
  };

  nixpkgs.config.allowUnfree = true;

  qt5 = {
    enable = true;
    platformTheme = "gtk2";
    style = "gtk2";
  };
  
  services.gnome.gnome-keyring.enable = true;
 
  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = true;
  
  time = {
    hardwareClockInLocalTime = true;
    timeZone = "Europe/Oslo";
  };
  
  virtualisation = {
    docker.enable = true;
    # virtualbox.host = {
    #   enable = true;
    #   enableExtensionPack = true;
    # };
  };
}
