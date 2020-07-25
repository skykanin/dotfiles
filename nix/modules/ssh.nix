{ config, pkgs, ... }:
{
 # Enable the OpenSSH daemon.
  services.openssh = {
    authorizedKeysFiles = [
      "$HOME/.ssh/id_rsa"
      "$HOME/.ssh/id_rsa_github"
      "$HOME/.ssh/hetzner_rsa"];
    enable = true;
  };
}
