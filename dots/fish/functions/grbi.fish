# Defined in - @ line 1
function grbi --wraps='git rebase -i' --description 'alias grbi=git rebase -i'
  git rebase -i $argv;
end
