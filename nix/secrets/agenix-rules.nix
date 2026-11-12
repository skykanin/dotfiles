let
  # /home/skykanin/.ssh/id_ed25519.pub
  skykanin = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJPwc9F2FtVrpLu1cDARzUKLLxba8p440ncikKwYDYdG";
  # /etc/ssh/ssh_host_ed25519_key.pub
  emma = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFQt7KRFXs9p/K7nNm03UR36TwPDRuVxpbihuK6yl+YE";
in {
  "noctalia-icloud.age".publicKeys = [skykanin emma];
  "noctalia-calendar.toml.age".publicKeys = [skykanin emma];
  "rclone.conf.age".publicKeys = [skykanin emma];
}
