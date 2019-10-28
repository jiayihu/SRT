# Real-time systems 

## Building like a true man

```bash
gprbuild --target=arm-eabi -d -P flasher.gpr src/led_flasher.adb -largs -Wl,-Map=map.txt
arm-eabi-objdump obj/led_flasher -h
arm-eabi-objcopy -O binary obj/led_flasher obj/led_flasher.bin
st-flash --reset write /home/parallels/Desktop/SRT/obj/gee.bin 0x08000000
```
