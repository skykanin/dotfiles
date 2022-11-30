function hshell --description "Utility function for adding ghc with packages to your environment"
    # Parse the ghc argument
    argparse 'ghc=?' -- $argv

    # Set default value for ghc argument
    if [ -z "$_flag_ghc" ]
       set -f _flag_ghc "925"
    end

    nix shell \
      --expr "(__getFlake \"nixpkgs\").legacyPackages.\${__currentSystem}.haskell.packages.ghc$_flag_ghc.ghcWithPackages (p: with p; [ $argv ])" \
      --impure \
      --command fish
end
