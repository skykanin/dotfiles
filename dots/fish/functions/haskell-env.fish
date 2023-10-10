function haskell-env -d "Load haskell repl with packages"
  nix shell --impure --expr \
    "(__getFlake \"nixpkgs\")
     .legacyPackages
     .\${__currentSystem}
     .haskellPackages
     .ghcWithPackages (p: with p; [ $argv ])" \
    --command ghci
end
