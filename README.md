Odefa
=====

Artifact for the paper **Higher-Order Demand-Driven Program Analysis**.

| | | |
|-|-|-|
| Leandro Facchinetti | <lfacchi2@jhu.edu> | The Johns Hopkins University |
| Zachary Palmer | <zachary.palmer@swarthmore.edu> | Swarthmore College |
| Scott F. Smith | <scott@jhu.edu> | The Johns Hopkins University |

Build
-----

1. Install [OCaml](https://ocaml.org/) and [OPAM](https://opam.ocaml.org/).

   <details>
   <summary>Windows Instructions</summary>

   Install [OCaml for Windows](http://fdopen.github.io/opam-repository-mingw/installation/), which includes the Cygwin shell with OCaml and OPAM preinstalled.

   </details>

2. Initialize OPAM:

   ```console
   $ opam init
   $ eval `opam config env`
   ```

3. Update & upgrade:

   ```console
   $ opam update
   $ opam upgrade
   ```

4. Switch to the appropriate compiler version:

   ```console
   $ opam switch 4.06.1
   $ eval `opam config env`
   ```

   <details>
   <summary>Windows Instructions</summary>

   Either

   ```console
   $ opam switch 4.06.1+mingw64
   $ eval `opam config env`
   ```

   or

   ```console
   $ opam switch 4.06.1+mingw32
   $ eval `opam config env`
   ```

   depending on the system.

   </details>

5. Install the dependencies:

   ```console
   $ opam install dune \
                  batteries \
                  menhir \
                  ounit \
                  ppx_deriving \
                  ppx_deriving_yojson \
                  "ocaml-monadic=0.4.0" \
                  monadlib \
                  "jhupllib=0.1.1" \
                  "pds-reachability=0.2.1"
   ```

   Note that newer versions of some of the constrained packages above may work, but the above-listed versions were tested with this project.

6. If your shell hashes binary locations, you may need to clear your hashes, for example (in Bash):

   ```console
   $ hash -r
   ```

7. Build:

   ```console
   $ make
   ```

8. Interact with the toploop (find sample programs at `test-sources/`):

   ```console
   $ ./ddpa_toploop
   ```

9. Run the tests:

   ```console
   $ make test
   ```

`toploop_main.native` Command-Line Arguments
--------------------------------------------

- `--log=trace`: Enable verbose logging.
- `--disable-inconsistency-check`: By default, the toploop checks programs for inconsistencies. For example, it checks that only functions appear in the operator position of a function call, and that only records appear in the subject position of a record projection. This inconsistency check forces variable lookups that interfere with benchmarking, and this flag disables it.
- `--select-context-stack=0ddpa`: Uses DDPA with a 0-level context stack (which is a monovariant analysis). Any positive integer value is admitted here (e.g. `7ddpa`).

Run the following for extended help (including options to produce diagrams of the incremental PDR graphs):

```console
$ ./toploop_main.native --help
```

Benchmarks
----------

1. Install [Odefa](#build).
2. Install [Racket](https://racket-lang.org) 6.12 or newer.
3. Install [Java](https://www.oracle.com/java/technologies/javase-downloads.html) 1.8 or newer.
4. Install [P4F](https://bitbucket.org/ucombinator/p4f-prototype).
5. Run `benchmark/run.sh`:
   ```console
   $ bash benchmark/run.sh
   ```
6. Aggregate results:
   ```console
   $ benchmark/resultsToCSV.py --dir benchmark/results
   ```

Developer Setup
---------------

Odefa depends on libraries which tend to develop at the same time as it does, but which are functionally independent and are designed to be used by other projects. Configure these libraries for local development by pinning them:

1. `jhupllib`:

   ```console
   $ git clone https://github.com/JHU-PL-Lab/jhu-pl-lib.git ../jhu-pl-lib
   $ opam pin add jhupllib ../jhu-pl-lib
   ```

2. `pds-reachability`:

   ```console
   $ git clone https://github.com/JHU-PL-Lab/pds-reachability.git ../pds-reachability
   $ opam pin add pds-reachability ../pds-reachability
   ```

When these libraries change, run

   ```console
   $ opam upgrade jhupllib pds-reachability
   ```
