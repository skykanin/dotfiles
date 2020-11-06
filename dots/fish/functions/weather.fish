# Defined in - @ line 1
function weather --wraps=curl\ \'wttr.in/\?M\?F\' --description alias\ weather=curl\ \'wttr.in/\?M\?F\'
  curl "wttr.in/$argv[1]?M?F" $argv[2..-1];
end
