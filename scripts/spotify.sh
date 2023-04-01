# Show Spotify current song

set -euo pipefail

spotifyon=$(pgrep spotify)
if [ -z "$spotifyon" ]; then
    echo ''
else
artist=$(dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify \
	/org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get \
	string:'org.mpris.MediaPlayer2.Player' string:'Metadata' \
	| grep -E -A 2 "artist" | grep -E -v "artist" \
	| grep -E -v "array" | cut -b 27-|cut -d '"' -f 1 \
        | grep -E -v ^$ | fribidi --nopad)

title=$(dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify \
       /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get \
       string:'org.mpris.MediaPlayer2.Player' string:'Metadata' \
       | grep -E -A 1 "title" | grep -E -v "title" |cut -b 44- \
       | cut -d '"' -f 1 | grep -E -v ^$ | fribidi --nopad)
    echo "$artist" "-" "$title"
fi
