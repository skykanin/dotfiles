# Defined in - @ line 1
function grs --wraps='git restore' --description 'alias grs=git restore'
  git restore $argv;
end
