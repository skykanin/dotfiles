# Defined in - @ line 1
function glgp --wraps='git log --stat -p' --description 'alias glgp=git log --stat -p'
  git log --stat -p $argv;
end
