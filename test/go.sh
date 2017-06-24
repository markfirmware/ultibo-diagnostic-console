#!/bin/bash

killall qemu-system-arm
qemu-system-arm -M versatilepb -cpu cortex-a8 -kernel artifacts/QEMUVPB/kernel.bin -m 256M -serial file:serial0.txt -serial stdio -usb -net nic -net user,hostfwd=tcp::5588-:80 -vnc :88,websocket=5788 &

sleep 3

zferentz/maproxy/usetornado.py
