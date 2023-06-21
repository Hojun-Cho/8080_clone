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
