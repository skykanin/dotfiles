function t --wraps='eza --group-directories-first --color-scale -g -laTh' --description 'Display directory as a tree with permissions and colours'
    eza --group-directories-first --color-scale -g -laTh $argv

end
