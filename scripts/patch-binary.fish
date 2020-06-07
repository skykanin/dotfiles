#!/usr/bin/env fish
function help_exit --description 'Help command'
  echo 'Usage: ./patch-binaries [arguments]'
  echo 'Arguments:'
  echo '(-b | --bin) <binary> : Pass the path to the binary blob to be patched'
  exit 1
end

argparse -N 1 'b/bin' -- $argv
or help_exit

set -lx res '['

echo 'Missing libraries:'
echo '---------------------'
for s in (ldd $argv | grep 'not found')
  set -lx clean (string trim $s | string replace ' => not found' '')
  for l in (nix-locate -1 -w "lib/$clean")
      if test (echo $l | head -c 1 | string trim) != '('
        echo $l
        set -lx cleanline (string replace '.out' '' $l)
        if test ! (string match "*$cleanline*" $res)
          set -a res $cleanline
        end          
      end
  end  
  echo '---------------------'
end

set -a res ']'
echo 'Libraries to add:'
echo $res
set -xl rpath (nix eval "(let pkgs = import <nixpkgs> {}; in with pkgs; lib.makeLibraryPath $res)")

patchelf --set-interpreter (cat $NIX_CC/nix-support/dynamic-linker) --set-rpath $rpath $argv
echo '----------------------------------'
echo '|                                |'
echo '| Binary successfully patched :) |'
echo '|                                |'
echo '----------------------------------'
