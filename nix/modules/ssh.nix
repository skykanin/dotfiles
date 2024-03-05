{
  lib,
  config,
  pkgs,
  ...
}: {
  programs.ssh = {
    startAgent = true;
    hostKeyAlgorithms = ["ssh-ed25519" "ssh-rsa"];
    extraConfig = ''
      AddKeysToAgent yes
    '';
  };
  environment.sessionVariables = lib.mkIf config.programs.ssh.startAgent {
    "SSH_AUTH_SOCK" = "/run/user/1000/ssh-agent";
  };

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
