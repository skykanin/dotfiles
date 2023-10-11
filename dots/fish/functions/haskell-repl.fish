function haskell-repl --description "Open ghc repl with packages"
    # Parse the ghc argument
    argparse 'v/ghc=' -- $argv

    # Set default value for ghc argument
    if [ -z $_flag_ghc ]
       set -f _flag_ghc '94'
    end

    nix shell --impure --expr \
      "(__getFlake \"nixpkgs\")
      .legacyPackages
      .\${__currentSystem}
      .haskell.packages.ghc$_flag_ghc
      .ghcWithPackages (p: with p; [ $argv ])" \
      --command ghci
end
