{
  config,
  pkgs,
  ...
}: {
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
