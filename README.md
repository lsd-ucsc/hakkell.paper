# Exceptional Actors (Hakkell)

The paper (`main.lhs`) is a literate haskell file.



## Import the program

Make sure to use `send` and `run` in your programs. The other functions
(`sendStatic`, `runStatic`, and `runDyn`) aren't meant to be used directly.



## Run the program

The main method supports two modes of operation.

### Run demo mode

If the environment variable `DEMO` is set to a number,
the following routines are run with the given number of nodes:

1. actor-based: Ring leader election
2. actor-based: Extended ring leader election
3. actor-based: benchRing election (extended ring leader election that reports termination)
4. channel-based: channelRing election (extended ring leader election that reports termination)

### Run benchmark mode

If the environment variable `DEMO` is not set,
then the criterion benchmark will run.
It does an extended ring leader election with both the
actor-based (benchRing) and channel-based (channelRing) implementations.
The criterion benchmark can be configured to
run (up to) different ring sizes by setting
the environment variable `BENCH` to power of 2.



## Benchmark the program

Running in benchmark mode (above) isn't sufficient to get a consistent result.
The included makefile has a target `make bench` which attempts to control for
various factors:
It uses python to remove printlines from `main.lhs`,
it builds an executable that defaults to `+RTS -N4`,
and
it executes `benchprep.sh` to set your CPU (if on linux) to 1.6GHz
and turn of frequency scaling.
After that, it runs benchmark mode.

If you run `make bench`, feel free to zero out `benchprep.sh` first.
The output from `make bench` is `make.bench.csv` and `make.bench.html`.



## Build the program

We developed this program on NixOS, and recommend using `nix-shell` to enter
an environment where the `makefile` does the rest.

* `nix-shell --run 'make'` -- Build the paper
* `nix-shell --run 'make bench'` -- Run the benchmark
* `nix-shell --run 'make perf'` -- Obtain an eventlog

For users not on NixOS, a cabal-file and a cabal-freeze-file are included.
The `makefile` isn't aware of the cabal setup.
