{
  config,
  pkgs,
  inputs,
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
    networking = {
      firewall.enable = true;
      networkmanager.enable = true;
    };

    nix.settings = {
      extra-substituters = [
        "https://iohk.cachix.org"
        "https://nix-gaming.cachix.org"
        "https://nix-citizen.cachix.org"
      ];
      extra-trusted-public-keys = [
        "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
        "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
        "nix-citizen.cachix.org-1:lPMkWc2X8XD4/7YPEEwXKKBg+SVbYTVrAaLA2wQTKCo="
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

  programs.niri.enable = true;

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
        libva-vdpau-driver
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

    # Enable COSMIC desktop environment
    desktopManager.cosmic.enable = true;
    displayManager.cosmic-greeter.enable = true;
    # Use GNOME keyring
    gnome.gnome-keyring.enable = true;

    gvfs = {
      enable = true;
      package = pkgs.gvfs.override { gnomeSupport = true; };
    };

    sunshine = {
      enable = true;
      autoStart = true;
      capSysAdmin = true;
      openFirewall = true;
      applications = {
        env = {
          PATH = "$(PATH):${config.programs.steam.package}/bin";
        };
        apps = [
          {
            name = "Steam Big Picture";
            cmd = "${config.programs.steam.package}/bin/steam -bigpicture";
            image-path = "steam.png";
            prep-cmd = [
              {
                do = "";
                undo = "${config.programs.steam.package}/bin/steam -shutdown";
              }
            ];
          }
          {
            name = "Desktop";
            image-path = "desktop.png";
          }
        ];
      };
    };

    udev.extraRules = ''
      ## Controller support for Sunshine.
      KERNEL=="uinput", GROUP="input", MODE="0660" OPTIONS+="static_node=uinput"
    '';

    # For better performance in CosmicDE
    system76-scheduler.enable = true;
  };

  networking = {
    hostName = "emma";
    useDHCP = false;
    interfaces.wlan0.useDHCP = true;
  };

  environment = {
    cosmic.excludePackages = with pkgs; [
      cosmic-edit
      cosmic-player
      cosmic-term
    ];

    systemPackages = with pkgs; [
      # android-studio
      # android-udev-rules
      adwaita-icon-theme
      ani-cli
      chatterino2
      clj-kondo
      discord
      faugus-launcher
      google-chrome
      inputs.nix-citizen.packages.${pkgs.system}.star-citizen
      jetbrains-toolbox
      lagrange
      metals
      networkmanagerapplet
      obs-cmd
      obs-studio-custom
      qbittorrent
      racket
      rclone
      scala_3
      tokyonight-gtk-theme
      unison-ucm
      webos-dev-manager
      wl-clipboard
      wineWow64Packages.waylandFull
    ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?
}
