{ config, pkgs, authorizedSshKeyFiles, ... }: {
  # Enable the OpenSSH daemon.
  services.openssh = {
    kbdInteractiveAuthentication = false;
    passwordAuthentication = true;
    authorizedKeysFiles = let sshPath = filename: "$HOME/.ssh/${filename}";
    in map sshPath authorizedSshKeyFiles;
    enable = true;
    permitRootLogin = "without-password";
    allowSFTP = false;
    hostKeys = [{
      path = "/etc/ssh/ssh_host_ed25519_key";
      type = "ed25519";
    }];
    extraConfig = ''
      Subsystem sftp internal-sftp
      XAuthLocation none
    '';
  };
}
