{
  i3lock,
  writeShellScript,
  time,
}: let
  lock-script = writeShellScript "lock-script" ''
    ${i3lock}/bin/i3lock -c 000000 -e -f -i ~/Pictures/wallpapers/evening-forest2k.png
  '';
in {
  enable = true;
  inherit time;
  locker = toString lock-script;
  nowlocker = toString lock-script;
}
