# Defined in - @ line 1
function gd --wraps='git diff' --description 'alias gd=git diff'
  git diff $argv;
end
