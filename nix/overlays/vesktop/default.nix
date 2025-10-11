final: prev: {
  vesktop = prev.vesktop.overrideAttrs {
    postInstall = ''
      install -Dm0644 ${./discord.png} $out/share/icons/hicolor/256x256x/apps/discord.png
    '';

   desktopItems = final.lib.optional final.stdenv.hostPlatform.isLinux (final.makeDesktopItem {
     name = "vesktop";
     desktopName = "Vesktop";
     exec = "vesktop %U";
     icon = "discord";
     startupWMClass = "Vesktop";
     genericName = "Internet Messenger";
     keywords = [
       "discord"
       "vencord"
       "electron"
       "chat"
     ];
     categories = [
       "Network"
       "InstantMessaging"
       "Chat"
     ];
     });
  };

}
