# Real-time systems 

## Building like a true man

```bash
$ gprbuild --target=arm-eabi -f -d -P gee.gpr src/gee.adb -largs -Wl,-Map=map.txt

# Only to run the application without debugging
$ arm-eabi-objcopy -O binary obj/gee obj/gee.bin
$ st-flash --reset write obj/gee.bin 0x08000000
```

## Debugging

```bash
$ st-util --semihosting
```

In a different tab run the following command. This will automatically use the commands defined in `.gdbinit`.

```bash
$ arm-none-eabi-gdb

(gdb) continue
```

If you have just recompiled the application and want to load on the board without exiting from gdb:

```bash
(gdb) source .gdbinit
(gdb) continue
```

## Compiling the runtime
fps scheduling:
```bash
runtime$ gprbuild -P ravenscar_full_stm32f429disco_pork.gpr -p
runtime$ gprinstall -P ravenscar_full_stm32f429disco_pork.gpr -p -f
```
edf scheduling:
```bash
runtime$ gprbuild -P ravenscar_full_stm32f429disco_pork.gpr -p -Xsched=edf
runtime$ gprinstall -P ravenscar_full_stm32f429disco_pork.gpr -p -f
```

