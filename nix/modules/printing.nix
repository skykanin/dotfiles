{
  config,
  pkgs,
  ...
}: {
  services.printing = {
    enable = false;
    drivers = [pkgs.brlaser pkgs.hplipWithPlugin];
  };
}
