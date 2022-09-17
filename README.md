# fROCtals

ASCII mandelbrot plotter in Roc, with comparison implementations in other languages

Comparisons implemented: 

- [x] JavaScript
- [x] Rust
- [ ] Haskell
- [ ] Zig
- [ ] Go

## Roc Setup

This uses the cli-platform from the roc-lang/roc repo examples. Tested working 
as of 09/17/2022. I simply created a symlink to cli-platform.

You will need to install the latest Roc nightly build or build from source. 
Instructions available at roc-lang/roc.

Run the program: `roc run mandelbrot.roc`

Create optimized build: `roc build mandelbrot.roc --optimize`

## JS Implementation

JS implementation tested with Node v16.


## Benchmark strategy

All implementations use a naive escape-time algorithm. All implementations 
generate a plot of the same size. All implementations have max iterations set
to 1M to magnify the differences between languages.

1. Create optimized binaries for compiled languages.
2. Time runs using `time ./executable` or `time interpreter ./src-file`
