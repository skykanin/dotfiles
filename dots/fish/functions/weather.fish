# Defined in - @ line 1
function weather --wraps=curl\ \'wttr.in/\?M\?F\' --description alias\ weather=curl\ \'wttr.in/\?M\?F\'
  curl 'wttr.in/?M?F' $argv;
end
