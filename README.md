# Real-time systems 

## Building like a true man

```bash
$ gprbuild --target=arm-eabi -f -d -P gee.gpr src/gee.adb -largs -Wl,-Map=map.txt
$ arm-eabi-objcopy -O binary obj/gee obj/gee.bin
$ st-flash --reset write obj/gee.bin 0x08000000
```

## Debugging

```bash
$ st-util --semihosting
```

In a different tab:

```bash
$ arm-none-eabi-gdb

(gdb) continue
```

## Compiling the runtime

```bash
gprbuild -P ravenscar_full_stm32f429disco_pork.gpr
runtime$ gprinstall -P ravenscar_full_stm32f429disco_pork.gpr -p -f
```
