# Defined in - @ line 1
function gr --wraps='git remote' --description 'alias gr=git remote'
  git remote $argv;
end
