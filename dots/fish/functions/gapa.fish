# Defined in - @ line 1
function gapa --wraps='git add --patch' --description 'alias gapa=git add --patch'
  git add --patch $argv;
end
