{ config, pkgs, ... }:

{
  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    # Only the full build has Bluetooth support, so it must be selected here.
    package = pkgs.pulseaudioFull;
  };
}
