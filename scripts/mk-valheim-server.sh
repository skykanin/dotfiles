docker run -d \
    --name valheim-server \
    --cap-add=sys_nice \
    --stop-timeout 120 \
    -p 2456-2457:2456-2457/udp \
    -v /root/valheim-server/config:/config \
    -v /root/valheim-server/data:/opt/valheim \
    -e SERVER_NAME="Dreamtopia" \
    -e BACKUPS_MAX_COUNT=2 \
    -e WORLD_NAME="Dreamtopia" \
    -e SERVER_PASS="nootnoot" \
    -e VALHEIM_PLUS=true \
    -e UPDATE_CRON="0 0 * * *" \
    lloesche/valheim-server
