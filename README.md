# 8080_clone

### parity bit how to work
* I think using gcc builtin parity is better.
```sh
    x ^= x >> 4
    x ^= x >> 2
    x ^= x >> 1
    return x & 1
```

### SBB, SUB
* cf flag mean carry flag. Thus if cf flag is 0 then can't borrow.
* In SBB, 'B' means borrow. If cf flag is 1 then can't borrow from left.
    But, If cf flag is 0 then can borrow from left.

### Restarrt RST 
* RST0 ~ RST7
```c
    push current "pc" to stack
	i8080_push(c, c->pc); 
    jump to "0x00 ~ 0xff"
    0x00 : interrupt vector to 0x0000
    0x08 : interrupt vector to 0x0008
    ...
    0xff : interrupt vector to 0x0038
	i8080_jmp(c, addr);
```
* 0x00 ~ 0x38 aread is excutable area

### Interrupt
* 8080 has an Interrupt line(pin 14)
* Internally the processor has Interrupt Enable bit.
* Two instrctuons, EI, DI set and clrear this bit.
* when a device issues an interrupt, the processor responds with an "interrupt acknowledge"(~INTA)
* The signal has the same timing as th "Memory Read"(~MEMR) signal and it is intened trigger
    the peripheral device to place a "Restart" instruction on the data bus.
* Restart instructions is RST0 ~ RST7


