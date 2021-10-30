{ pkgs, enable, time }:

let
  lock-script = pkgs.writeShellScript "lock-script" ''
    ${pkgs.i3lock-color}/bin/i3lock-color -k -u -c 00000000 --date-str='%A, %B %Y' \
      --time-color=ff005acc --date-color=ff005acc --time-size=60 --date-size=20
  '';
in {
  inherit enable time;
  locker = toString lock-script;
  nowlocker = toString lock-script;
}
