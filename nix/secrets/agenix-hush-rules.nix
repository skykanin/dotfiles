let
  # /home/simon/.ssh/id_ed25519.pub
  simon = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILTfJ93dRnPodxfgrAn6NN/5DhtMBV4wAnGozn1I4vO6 id";
  # /etc/ssh/ssh_host_ed25519_key.pub
  hush = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOeUsddYhrYu9Y1cc4JXFDdD5PqaosuIeLiNvRNgp2aj root@nixos";
in {
  "cloudflared-tunnel-token.age".publicKeys = [simon hush];
}
