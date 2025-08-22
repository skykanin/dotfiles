{
  config,
  options,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../modules/bluetooth.nix
    ../modules/boot-efi.nix
    ../modules/nix.nix
    ../modules/emacs.nix
    ../modules/keyboard.nix
    ../modules/general/default.nix
    ../modules/general/nixos.nix
    ../modules/networking.nix
    ../modules/packages.nix
    ../modules/printing.nix
    ../modules/programs/default.nix
    ../modules/programs/nixos.nix
    ../modules/services/media.nix
    ../modules/services/noisetorch.nix
    ../modules/redshift.nix
    ../modules/sound.nix
    ../modules/ssh.nix
    ../modules/user.nix
    ../modules/desktop/wayland/hyprland.nix
    ../modules/desktop/wayland/sway.nix
    ../modules/desktop/wayland/greetd.nix
  ];

  # Local modules
  local = {
    desktop = {
      sway = {
        enable = true;
        xwayland.enable = true;
      };
    };

    networking = {
      firewall.enable = true;
      networkmanager.enable = true;
    };

    nix = {
      extra-substituters = [
        "https://iohk.cachix.org"
      ];
      extra-trusted-public-keys = [
        "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      ];
    };

    programs.steam.enable = true;

    services = {
      media.enable = true;
      noisetorch = {
        enable = true;
        device-id = "alsa_input.usb-Blue_Microphones_Yeti_Stereo_Microphone_REV8-00.analog-stereo";
        device-unit = "sys-devices-pci0000:00-0000:00:08.1-0000:13:00.4-usb5-5\x2d2-5\x2d2:1.0-sound-card4-controlC4.device";
      };
    };
  };

  # Use Norwegian locale
  i18n.extraLocaleSettings = {
    LC_MEASUREMENT = "nb_NO.UTF-8";
    LC_MONETARY = "nb_NO.UTF-8";
    LC_PAPER = "nb_NO.UTF-8";
    LC_NAME = "nb_NO.UTF-8";
    LC_NUMERIC = "nb_NO.UTF-8";
    LC_TELEPHONE = "nb_NO.UTF-8";
    LC_TIME = "nb_NO.UTF-8";
    LC_COLLATE = "nb_NO.UTF-8";
  };

  hardware = {
    graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = with pkgs; [
        vulkan-loader
        vulkan-tools
        # vulkan-validation-layers
        # vulkan-extension-layer
        vaapiVdpau
      ];
    };
    opentabletdriver.enable = true;
  };

  services = {
    flatpak.enable = true;

    # Enable USB multiplexing daemon, needed for ifuse.
    usbmuxd.enable = true;

    # Enable AMD GPU drivers
    xserver.videoDrivers = ["amdgpu"];

    # Enable GNOME desktop manager
    udev.packages = [pkgs.gnome-settings-daemon];
    desktopManager.gnome.enable = true;
    displayManager.gdm.enable = true;
  };

  networking = {
    hostName = "emma";
    useDHCP = false;
    interfaces.wlan0.useDHCP = true;
  };

  environment.systemPackages = with pkgs; [
    # android-studio
    # android-udev-rules
    adwaita-icon-theme
    ani-cli
    bottles
    chatterino2
    clj-kondo
    discord
    google-chrome
    gnomeExtensions.appindicator
    gnomeExtensions.dash-to-dock
    jetbrains-toolbox
    lagrange
    lutris
    metals
    networkmanagerapplet
    obs-studio-custom
    qbittorrent
    racket
    rclone
    scala_3
    tokyonight-gtk-theme
    unison-ucm
    wineWowPackages.waylandFull
  ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?
}
