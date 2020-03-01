{ config, pkgs, ... }:

{
  services.redshift = {
    enable = true;
    temperature = {
      day = 6500;
      night = 3000;
    };
  };

  location = {
    latitude = 63.430515;
    longitude = 10.395053;
  }; 
}
