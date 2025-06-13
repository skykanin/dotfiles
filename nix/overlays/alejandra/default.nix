final: prev: {
  alejandra = prev.alejandra.overrideAttrs (pFinal: pPrev: {
    # Removes annoying ads in CLI
    patches = pPrev.patches ++ [./remove-ads.patch];
    doCheck = false;
  });
}
