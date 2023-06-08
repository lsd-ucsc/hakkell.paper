# Exceptional Actors (Hakkell)

The paper (`main.lhs`) is a literate haskell program.



## Import the program

Make sure to use `send` and `run` in your programs. The other functions
(`sendStatic`, `runStatic`, and `runDyn`) aren't meant to be used directly.



## Run the program

The main method supports two modes of operation.
For either mode, set the environment variable `RING_SIZE`
to determine how many nodes will participate in the election.

### Run demo mode

Set the environment variable `MODE` to `actors`,`channels`, or `control`
to run either the `benchActors`, `benchChannels` or `benchControl` functions
respectivly, on the specified `RING_SIZE` nodes.

This is the mode we used for measuring memory use.

### Run benchmark mode

If the environment variable `MODE` is not set,
then the criterion benchmark will run.
It sets up a benchmark group for the specified `RING_SIZE`
containing a becnhmark of `benchControl`, `benchActors`, and `benchChannels`.

This is the mode we used for measuring run time.
We used a shell script to select and run only one of those functions at a time.



## Benchmark the program

Running in benchmark mode (above) isn't sufficient to get a consistent result.
The included makefile has a target `make bench` which attempts to control for
various factors:
It uses python to remove printlines from `main.lhs`,
it builds an executable that defaults to `+RTS -N4`,
and
it executes `benchprep.sh` to set your CPU (if on linux) to 1.6GHz
and turn off frequency scaling.
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
