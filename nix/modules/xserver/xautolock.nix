{ pkgs, enable, time }:

let
  lock-script = pkgs.writeShellScript "lock-script" ''
     ${pkgs.i3lock}/bin/i3lock -c 000000 -e -f -i ~/Pictures/wallpapers/evening-forest2k.png
  '';
in {
  inherit enable time;
  locker = toString lock-script;
  nowlocker = toString lock-script;
}
