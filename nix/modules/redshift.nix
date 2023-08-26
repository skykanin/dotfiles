{
  config,
  pkgs,
  ...
}: {
  services.redshift = {
    enable = true;
    temperature = {
      day = 7000;
      night = 3500;
    };
  };

  location = {
    latitude = 59.913868;
    longitude = 10.752245;
  };
}
