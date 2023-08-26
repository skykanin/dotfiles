{
  config,
  pkgs,
  ...
}: {
  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
    # Only the full build has Bluetooth support, so it must be selected here.
    package = pkgs.pulseaudioFull;
    # Disable power saving
    extraConfig = "unload-module module-suspend-on-idle";
  };
}
