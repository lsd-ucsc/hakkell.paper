# Exceptional Actors (Hakkell)

This project explores the use of *asynchronous exceptions* as an inter-thread
communication mechanism in GHC Haskell. We implemented the actor model of
programming on top of asynchronous exceptions as a small framework:
a send function and a message-receipt mainloop (to be run on each thread).
The paper (`main.lhs`) is written as a Literate-Haskell program and was
accepted at Haskell Symposium 2023. To read the paper, we recommend rendering
it first or visiting <https://doi.org/10.1145/3609026.3609728>.



## Build the program

1. ### NixOS users

   We developed this program on NixOS, and recommend using `nix-shell`
   to enter an environment where the `makefile` does the rest.

   * `nix-shell --run 'make'` -- Build the paper
   * `nix-shell --run 'make main.bench.elf'` -- Build the benchmark executable
   * `nix-shell --run 'make bench'` -- Build and run the benchmark (first read all of [*Benchmark the program*](#benchmark-the-program))
   * `nix-shell --run 'make prof'` -- Obtain an eventlog

1. ### Cabal users

   For users not on NixOS, we include a cabal-file and a cabal-freeze-file.
   We built the project on GHC `9.0.2` with `base-4.15.1.0` using Cabal `3.10.1.0`.

   ```sh-session
   $ cabal update
   ...
   $ cabal v2-build
   ...
   Linking /.../hakkell.paper/dist-newstyle/build/x86_64-linux/ghc-9.0.2/hakkell-paper-0.0.0/x/main/build/main/main ...
   $ cabal v2-exec make main.bench.elf
   ...
   [1 of 1] Compiling Main             ( main.noprint.lhs, main.noprint.o )
   Linking main.bench.elf ...
   rm main.noprint.lhs
   ```

   The `makefile` isn't aware of the `cabal exec` environment
   and some targets have additional dependencies,
   so not all the targets will work under `cabal exec`
   (the clean targets do not work, for example).



## Import the program

Make sure to use `send` and `run` in your programs. The other functions
(`sendStatic`, `runStatic`, and `runDyn`) aren't meant to be used directly.



## Run the program

The main method supports two modes of operation.
For either mode, set the environment variable `RING_SIZE`
to determine how many nodes will participate in the election.

1. ### Run demo mode

   Set the environment variable `MODE` to `actors`, `channels`, or `control`
   to run `benchActors`, `benchChannels`, or `benchControl` once,
   on the specified `RING_SIZE` nodes.

   This is the mode we used for measuring total bytes allocated.

1. ### Run benchmark mode

   If the environment variable `MODE` is not set,
   then the criterion benchmark will run.
   It sets up a benchmark group for the specified `RING_SIZE`
   containing a becnhmark of `benchControl`, `benchActors`, and `benchChannels`.

   This is the mode we used for measuring running time.
   We used a shell script that provided command-line arguments interpreted
   by criterion to select and run only one of those functions at a time.



## Benchmark the program

An extra step is required to obtain a consistent result from benchmark mode
(above).
Printlines must be stripped from the source code.
The included makefile has targets which attempt to control
for this and other factors that may confound benchmark results.

* `make main.bench.elf`

  * This target uses a python script to remove printlines from `main.lhs` and
    compiles an executable which defaults to `+RTS -N4`.
    * You may override the capabilities default with an environment variable.
      For example, an `+RTS -N` default is obtained this way:  
      `env CAPABILITIES= make main.bench.elf`
  * Running executables produced by this target is sufficient to get a
    consistent result on Amazon AWS EC2 instances.

* `make bench`

  * This target builds an executable and performs additional steps before
    running benchmark mode.
    * It attempts to control for various factors on a personal laptop running
      linux that may confound benchmark results.
    * It executes `benchprep.sh` to set your CPU to 1.6GHz,
      turns off frequency scaling, stops TLP & ThermalD, and disables Intel Pstate.
    * If you run `make bench`, first audit `benchprep.sh` or zero it out.
  * The output from `make bench` is `make.bench.csv` and `make.bench.html`.
