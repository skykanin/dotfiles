# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let unstable = import <nixos-unstable> { config.allowUnfree = true; };
    nixpkgs-unstable = import <nixpkgs-unstable> { config.allowUnfree = true; }; in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "daisy"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;
  hardware.brightnessctl.enable = true;
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  programs.fuse.userAllowOther = true;

  # Set your time zone.
  time.timeZone = "Europe/Oslo";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    arc-icon-theme
    arc-theme
    ant-theme
    cabal-install
    clojure
    curl
    unstable.discord
    docker
    (import /home/skykanin/Documents/emacs.nix { inherit pkgs; }) # Emacs with my pkgs
    firefox
    fish
    ghc
    gnome3.networkmanagerapplet
    git
    joker
    kitty
    my-idea-ultimate
    my-leiningen
    lxappearance-gtk3
    neofetch
    nitrogen
    pamixer
    playerctl
    (python3.withPackages (ps: with ps; [
      pywal
    ]))
    pywal
    rofi
    spotifywm
    wget
    xdg_utils
    xorg.xbacklight
  ];

  fonts.fonts = with pkgs; [
    fira-code
    fira-code-symbols
    nerdfonts
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    authorizedKeysFiles = [ "$HOME/.ssh/id_rsa" "$HOME/.ssh/id_rsa_github"];
    enable = true;
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    # Only the full build has Bluetooth support, so it must be selected here.
    package = pkgs.pulseaudioFull;
  };

  # Suspend on lid close
  services.logind.lidSwitch = "suspend";

  # Nixpkgs overlays
  nixpkgs.overlays = [
    (self: super: {
      my-polybar = super.polybar.override {
        i3Support = true;
        # i3GapsSupport = true;
        pulseSupport = true;
      };
    })
    (self: super: {
      my-jbr = super.jetbrains.jdk.override {
        url = "https://jetbrains.bintray.com/intellij-jdk/jbr-11_0_2-linux-x64-b164.tar.gz";
      };
    })
    (self: super: {
      my-leiningen = super.leiningen.override {
        jdk = pkgs.openjdk11;
      };
    })
    (self: super: {
      my-idea-ultimate = super.jetbrains.idea-ultimate.override {
        jdk = pkgs.jetbrains.jdk;
      };
    })
  ];

  # Enable the X11 windowing system.
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;
  
  services.compton = {
    backend = "glx";
    enable = true;
    refreshRate = 0;
    activeOpacity = "1.0";
    inactiveOpacity = "1.0";
    
  };

  services.xserver = {
    autoRepeatDelay = 280;
    autoRepeatInterval = 200;
    enable = true;
    layout = "no";
    libinput = {
      enable = true;
      naturalScrolling = true;
    };
    autorun = true;
    
    windowManager.default = "i3";
    windowManager.i3 = {
      enable = true;
      configFile = /home/skykanin/.config/i3/config;
      package = pkgs.i3-gaps;
      extraPackages = with pkgs; [
                        rofi
                        my-polybar
                        betterlockscreen
                      ];
    };
    
    desktopManager = {
      default = "none";
      xterm.enable = false;
    };
  
    displayManager = {
      lightdm = {
        enable = true;
        autoLogin = {
          enable = true;
          user = "skykanin";
        };
        greeter.enable = false;
        greeters.gtk.indicators = [ "~host" "~spacer" "~clock" "~spacer" "~a11y" "~session" "~power" ];
      };
    };
  };
  
  services.redshift = {
    enable = true;
    temperature = {
      day = 6500;
      night = 3000;
    };
  };

  location = {
    latitude = 63.430515;
    longitude = 10.395053;
  };

  # Define systemd service for betterlockscreen to run on suspend/sleep
  # A bit janky cause I had to manually cache it the first time for root cache
  systemd.services.betterlockscreen = {
    enable = true;
    aliases = [ "betterlockscreen@skykanin.service" ];
    description = "Locks screen when going to sleep/suspend";
    environment = { DISPLAY = ":0"; };
    serviceConfig = {
      User = "skykanin";
      Type = "simple";
      ExecStart = ''${pkgs.betterlockscreen}/bin/betterlockscreen -l dim'';
      TimeoutSec = "infinity";
    };
    wantedBy = [ "sleep.target" "suspend.target" ];
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.skykanin = {
    isNormalUser = true;
    description= "Skykanin";
    extraGroups = ["wheel" "audio" "video" "networkmanager" "postgres"]; # Enable ‘sudo’ for the user.
    initialPassword = "skykanin";
    openssh.authorizedKeys.keyFiles = [ /home/skykanin/.ssh/id_rsa /home/skykanin/.ssh/id_rsa_github ];
    shell = pkgs.fish;
  };
  
  programs.fish = {
    enable = true;
    vendor = {
      completions.enable = true;
      config.enable = true;
      functions.enable = true;
    };
  };

  programs.java = {
    enable = true;
    package = pkgs.openjdk11;
  };

  programs.light.enable = true; 

  virtualisation.docker.enable = true;

  nixpkgs.config.allowUnfree = true;
  fonts.enableDefaultFonts = true;

  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

}
