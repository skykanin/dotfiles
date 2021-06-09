{ config, pkgs, ... }:
{
 # Enable the OpenSSH daemon.
  services.openssh = {
    challengeResponseAuthentication = false;
    passwordAuthentication = false;
    authorizedKeysFiles = [
      "$HOME/.ssh/id_rsa"
      "$HOME/.ssh/id_rsa_github"
      "$HOME/.ssh/hetzner_rsa"];
    enable = true;
    permitRootLogin = "without-password";
    allowSFTP = false;
    hostKeys = [
      {
        path = "/etc/ssh/ssh_host_ed25519_key";
        type = "ed25519";
      }
    ];
    extraConfig = ''
      Subsystem sftp internal-sftp
      XAuthLocation none
    '';
  };
}
