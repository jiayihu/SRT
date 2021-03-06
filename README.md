# Real-time systems 

## Building like a true man with CLI

```bash
$ gprbuild --target=arm-eabi -f -d -P gee.gpr -largs -Wl,-Map=map.txt
```

If you just wish to run the application without debugging and logs:

```
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

For FPS:

```bash
runtime$ gprbuild -P ravenscar_full_stm32f429disco_pork.gpr
runtime$ gprinstall -P ravenscar_full_stm32f429disco_pork.gpr -p -f
```

For EDF:

```bash
runtime$ gprbuild -P ravenscar_full_stm32f429disco_pork.gpr -Xsched=edf
runtime$ gprinstall -P ravenscar_full_stm32f429disco_pork.gpr -p -f -Xsched=edf
```
