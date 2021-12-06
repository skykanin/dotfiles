function gwb --wraps=git\ branch\ \|\ awk\ \'/^\\\*\ /\ \{\ print\ \ \}\' --wraps=git\ branch\ \|\ awk\ \"/^\\\*\ /\ \{\ print\ \$2\ \}\" --wraps=git\ branch\ \|\ awk\ \'/^\\\*\ /\ \{\ print\ \$2\ \}\' --description alias\ gwb=git\ branch\ \|\ awk\ \'/^\\\*\ /\ \{\ print\ \$2\ \}\'
  git branch | awk '/^\* / { print $2 }' $argv; 
end
