{
  config,
  pkgs,
  ...
}: {
  services.printing = {
    enable = true;
    drivers = [pkgs.brlaser pkgs.hplipWithPlugin];
  };
}
