function nixify --description 'Shell function to quickly setup nix + direnv in a new project'
  if test ! -e ./.envrc
    echo "use nix" > .envrc
    direnv allow
  end
  if test ! -e shell.nix
    echo > shell.nix "\
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.bashInteractive
  ];
}
"
    $EDITOR shell.nix
  end
end
