# 8080_clone

### parity bit how to work
* I think using gcc builtin parity is better.
```sh
    x ^= x >> 4
    x ^= x >> 2
    x ^= x >> 1
    return x & 1
```
