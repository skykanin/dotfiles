# Utility function for interacting with https://transfer.sh
function transfer --description "Utility function for interacting with https://transfer.sh"
    set -l filepath $argv[1]
    curl --progress-bar \
      --upload-file "$filepath" "https://transfer.sh/$(basename $filepath)" \
      | tee /dev/null \
      | xclip -selection clipboard
end
