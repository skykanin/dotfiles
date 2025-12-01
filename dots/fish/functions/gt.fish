function gt --wraps='git tag | sort -V' --wraps='git tag' --description 'alias gt git tag'
  git tag $argv
        
end
