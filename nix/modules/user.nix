{ config, pkgs, authorizedSshKeyFiles, ... }:

let username = "skykanin";
in {
  users.users.skykanin = {
    isNormalUser = true;
    description = username;
    extraGroups =
      [ "wheel" "audio" "docker" "video" "networkmanager" "postgres" ];
    initialPassword = "skykanin"; # Change with passwd later
    openssh.authorizedKeys.keyFiles =
      let sshPath = filename: "/home/${username}/.ssh/${filename}";
      in map sshPath authorizedSshKeyFiles;
    shell = pkgs.fish;
  };
}
