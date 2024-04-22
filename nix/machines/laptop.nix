{
  config,
  options,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ../modules/bluetooth.nix
    ../modules/boot-efi.nix
    ../modules/nix.nix
    ../modules/emacs.nix
    ../modules/hardware.nix
    ../modules/general/default.nix
    ../modules/general/nixos.nix
    ../modules/networking.nix
    ../modules/packages.nix
    ../modules/printing.nix
    ../modules/programs/default.nix
    ../modules/programs/nixos.nix
    ../modules/overlays.nix
    ../modules/redshift.nix
    ../modules/sound.nix
    ../modules/ssh.nix
    ../modules/user.nix
  ];

  # Local modules
  local = {
    hardware.opentabletdriver.enable = true;

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

    programs = {
      light.enable = true;
      steam.enable = true;
    };
  };

  services.udev.packages = with pkgs; [gnome.gnome-settings-daemon];

  # Gnome uses Wayland by default, the attrname is just legacy.
  services.xserver = {
    enable = true;
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };

  services.gnome.gnome-browser-connector.enable = true;

  environment.systemPackages = with pkgs; [
    flat-remix-icon-theme
    gnome.gnome-tweaks
    gnome.gnome-shell-extensions
    gnomeExtensions.appindicator
    gnomeExtensions.dash-to-dock
    paper-icon-theme
    vimix-icon-theme
    wl-clipboard
    wlr-randr
  ];

  hardware = {
    opengl.extraPackages = with pkgs; [
      intel-media-driver
    ];
    # Override gnome module setting this to true
    pulseaudio.enable = lib.mkForce false;
  };

  networking.hostName = "daisy";

  environment.variables = {MESA_LOADER_DRIVER_OVERRIDE = "daisy";};

  # Suspend on lid close
  services.logind.lidSwitch = "suspend";

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?
}
