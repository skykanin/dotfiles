# Defined in - @ line 1
function gba --wraps='git branch -a' --description 'alias gba=git branch -a'
  git branch -a $argv;
end
