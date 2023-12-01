# combinatorial-parsers

## Disclaimer

This implementation of combinatorial parsers has Proof of Concept status, there is large space for further optimizations and bug finding/fixing.

90% of idea credit should go to Georgiy Korneev. Idea was taken from Programming Paradigms course at ITMO University.

Yet its original implementation was made in Clojure, that's why there was an interest in making C++ implementation to investigate what hardships one can encounter while going this way.

## Build (ninja)

```sh
mkdir build
cd build
cmake -DCMAKE_BUILD_TYPE=Release -GNinja ..
ninja
```
