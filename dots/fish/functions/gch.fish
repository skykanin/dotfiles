# Defined in - @ line 1
function gch --wraps='git checkout' --description 'alias gch=git checkout'
  git checkout $argv;
end
