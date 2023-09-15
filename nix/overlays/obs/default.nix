final: prev: {
  obs-studio-custom = final.wrapOBS {
    plugins = with final.obs-studio-plugins; [input-overlay];
  };
}
