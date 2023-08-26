{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.local.hardware;
in {
  options.local.hardware = with lib; {
    opengl.enable =
      (mkEnableOption "Enable OpenGL") // {default = true;};
    opentabletdriver.enable =
      mkEnableOption "Enable opentable driver";
  };

  config.hardware = {
    opengl = {
      driSupport = true;
      enable = cfg.opengl.enable;
      extraPackages = with pkgs; [
        vaapiVdpau
        libvdpau-va-gl
        intel-media-driver
      ];
      # Override broken nvidia config which depends on 32 bit `pkgsi686Linux.nvidia-vaapi-driver`
      # for `opengl.driSupport32Bit` which is enabled by the steam config.
      # Relevant link:
      # https://github.com/NixOS/nixpkgs/blob/6d6682772b62652b5019ffd7572cea1f39b72b20/nixos/modules/hardware/video/nvidia.nix#L395C45-L395C45
      extraPackages32 = pkgs.lib.mkForce [pkgs.linuxPackages_latest.nvidia_x11.lib32];
    };
    opentabletdriver.enable = cfg.opentabletdriver.enable;
  };
}
