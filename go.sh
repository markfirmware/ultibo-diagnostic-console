#!/bin/bash

qemu-system-arm -M versatilepb -cpu cortex-a8 -kernel artifacts/QEMUVPB/kernel.bin -m 128M -serial stdio -serial vc -usb -net nic -net user,hostfwd=tcp::5588-:80 -vnc :88,websocket=5788
