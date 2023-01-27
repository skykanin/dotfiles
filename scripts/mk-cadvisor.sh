docker run \
  -v /:/rootfs:ro \
  -v /var/run:/var/run:ro \
  -v /sys:/sys:ro \
  -v /var/lib/docker/:/var/lib/docker:ro \
  -v /dev/disk/:/dev/disk:ro \
  -p 8080:8080 \
  -d \
  --name cadvisor \
  --privileged \
  --device /dev/kmsg \
  gcr.io/cadvisor/cadvisor:latest
