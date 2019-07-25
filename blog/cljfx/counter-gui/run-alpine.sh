#!/bin/sh

[[ -S /tmp/pulseaudio.xsocket ]] || pactl load-module module-native-protocol-unix socket=/tmp/pulseaudio.socket

xhost +"local:docker@"
xhost +local:root

docker run -it --rm \
       -e DISPLAY=$DISPLAY \
       -e NVIDIA_DRIVER_CAPABILITIES=compute,utility,graphics \
       -v /tmp/.X11-unix:/tmp/.X11-unix \
       -v /dev/snd:/dev/snd \
       -v /tmp/pulseaudio.socket:/tmp/pulseaudio.socket \
       -v /home/mcarter/src/eq-in-a-docker/pulseaudio.client.conf:/etc/pulse/client.conf \
       --device=/dev/snd:/dev/snd \
       --cpus=2 \
       --privileged \
       --name my_alpine_clojure_running \
       my-alpine-clojure:latest
