{
  lib,
  config,
  pkgs,
  ...
}: {
  programs.ssh = {
    startAgent = !config.services.gnome.gcr-ssh-agent.enable;
    hostKeyAlgorithms = ["ssh-ed25519" "ssh-rsa"];
    extraConfig = ''
      AddKeysToAgent yes
    '';
  };

  environment.sessionVariables.SSH_AUTH_SOCK =
    if config.services.gnome.gcr-ssh-agent.enable
    then "\${XDG_RUNTIME_DIR}/gcr/ssh"
    else "/run/user/1000/ssh-agent";

  # OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      KbdInteractiveAuthentication = false;
      PasswordAuthentication = false;
      PermitRootLogin = "without-password";
    };
    allowSFTP = false;
    extraConfig = ''
      Subsystem sftp internal-sftp
      XAuthLocation none
    '';
  };
}
