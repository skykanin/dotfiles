{ config, pkgs, ... }:

{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.skykanin = {
    isNormalUser = true;
    description = "Skykanin";
    extraGroups =
      [ "wheel" "audio" "docker" "video" "networkmanager" "postgres" ];
    initialPassword = "skykanin";
    openssh.authorizedKeys.keyFiles =
      [ "/home/skykanin/.ssh/id_rsa" "/home/skykanin/.ssh/id_rsa_github" ];
    shell = pkgs.fish;
  };
}
