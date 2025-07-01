# Artifact Evaluation: Scaling Instruction-Selection Verification with Authoritative ISA Semantics in an Industrial WebAssembly Compiler

## Introduction

Artifact accompanying our OOPSLA 2025 paper "Scaling Instruction-Selection
Verification with Authoritative ISA Semantics in an Industrial WebAssembly
Compiler".

Arrival is an instruction-selection verifier for the Cranelift production WebAssembly (Wasm)
compiler.  Our work verifies nearly all of the AArch64 instruction-selection rules
reachable from Wasm core. Furthermore, Arrival reduces the developer effort
required: 60% of all specifications benefit from our automation, thereby
requiring 2.6X fewer hand-written specifications than prior approaches.  Arrival
finds new bugs in Cranelift's instruction selection, and it is viable for
integration into production workflows.

Claims in the paper supported by this artifact:

* _Verification of nearly all AArch64 instruction-selection rules reachable from Wasm core._
  We show how to run the AArch64 verification run in Continuous Integration (CI) mode (lower timeout),
  and provide cached data from full verification runs.  We provide analysis
  scripts to produce the coverage statistics in Table 1.
* _Specification burden required by Arrival._
  Specifications are available in the Arrival codebase.  We provide the data and
  analysis scripts to support specifications statistics in Table 2.
* _Arrival's auto-generation of ISA specifications._
  We show how to run Arrival's ISA specification pipeline to reproduce all
  auto-generated ISA specifications, as detailed in Sections 5 and 6.2.  Our
  SymASLp fork of ASLp is open source and used in this artifact.
* _Arrival's suitability for integration into Continuous Integration (CI)._
  We show how to run the AArch64 verification run with settings suitable for CI,
  as discussed in Section 6.3.  We provide data and analysis scripts to
  reproduce the verification timings in Figure 5.
* _Arrival's ability to discover bugs in Cranelift._
  We show how to reproduce the `sdiv` lowering bug discussed in Section 2.4.

## Hardware Dependencies

This artifact requires a single x86 host machine for the best Docker performance
(we have tested it on an aarch64 M2 MacBook and the instructions all work, but due to 
poor virtualization performance with Docker, the CI run takes ~4 hours compared to 
~10-60 minutes on the x86 machines we've tested). 
While there is no strict 
requirement for many cores, the Arrival verifier parallelizes over available
cores, and reviewers will have a better time with at least 8 and ideally more
cores. 

## Getting Started Guide

### Setup [est. 10-30 minutes]

We provide our artifact as a
[Docker](https://docs.docker.com/engine/installation/) file. 

#### Install Docker if needed [est. 5 minutes]

Users should install [Docker][docker] based on their system's instructions. 

[docker]: https://docs.docker.com/engine/installation/

#### Optional: increase memory available to Docker  [est. 5 minutes] 

Our artifact performs better with >2GB of memory available (a limit imposed by some Docker configurations). To check how much memory is available to the running Docker container, open a second terminal on the host machine (not within the Docker shell) and run:
```
docker stats
```

Check the column titled `MEM USAGE / LIMIT`. On macOS/Windows, the second value of limit may initially be `1.939GiB`, because although native Docker on Linux [does not default to having memory limits][docker-mem], [Docker Desktop for Mac][docker-mac] and [Docker Desktop for Windows][docker-windows] have a 2GB limit imposed. 

You can increase the Docker Desktop memory limit to (e.g., to 4GB or 8GB) by following the links above or these steps:
1. Open Docker Desktop.
2. Open Setting (gear icon).
3. Click Resources on the side bar.
4. Increase the Memory bar.

Similarly, if you are running Docker within a virtual container or cloud instance, follow the instructions from that provider.

[docker-mem]: https://docs.docker.com/config/containers/resource_constraints/
[docker-mac]: https://docs.docker.com/desktop/mac/
[docker-windows]: https://docs.docker.com/desktop/windows/

We also suggest increasing the cores available to Docker to as many as your machine supports, ideally at least 8.

#### Build Docker instance [est. 5 minutes]

Build the `arrival` Docker image from the root of the artifact as follows:

```
./script/docker_build.sh
```

### Basic Test [est. 5 minutes]

Run the container with:
```
./script/docker_run.sh
```


> [!Note]  
> The remainder of this artifact assumes all commands are run from within the Docker instance.

#### Optional: open running instance in VSCode [est. 5 minutes]

We recommend connecting to the running Docker instance in [VSCode][] or another IDE that enables connecting to instances, so you can view files (including PDFs/images) interactively. Instructions for VSCode can be found here: [VSCode attach container][].

[vscode]: https://code.visualstudio.com
[VSCode attach container]: https://code.visualstudio.com/docs/devcontainers/attach-container

To verify a simple lowering of the integer addition `iadd` instruction, execute within the container:
```
./script/veri.sh -- --filter rule:iadd_base_case
```

This will build the verifier, and run it against all expansions of the
`iadd_base_case` rule. In this case there is only one expansion, and four
applicable type instantiations. You should see output similar to:

```
#111    iadd_base_case
        iadd({bits: int}, bv 8, bv 8) -> bv 8
                type solution status = solved
                applicability = applicable
                verification = success
        iadd({bits: int}, bv 16, bv 16) -> bv 16
                type solution status = solved
                applicability = applicable
                verification = success
        iadd({bits: int}, bv 32, bv 32) -> bv 32
                type solution status = solved
                applicability = applicable
                verification = success
        iadd({bits: int}, bv 64, bv 64) -> bv 64
                type solution status = solved
                applicability = applicable
                verification = success
        iadd({bits: int}, bv 128, bv 128) -> bv 128
                type solution status = solved
                applicability = inapplicable
```

## Step by Step Instructions

### Reproduce `sdiv` Bug [est. 5 mins]

Next, we'll reproduce the `sdiv` bug discussed in Section 2.4 "Bug Discovery with
Arrival".

Since the bug has now been fixed, we'll first revert the change to reintroduce the error. From within the container, run:
```
patch -R -p1 -d /root/arrival/ -i /root/artifact/data/sdiv_fix.patch
```

Run verification against the `sdiv` rule:
```
./script/veri.sh -- --filter rule:sdiv
```

After a few seconds, you should see a verification error as well as the values
of all intermediate terms in a counterexample, such as below (trimmed for brevity).

```
#416    sdiv
        sdiv({bits: int}, bv 8, bv 8) -> bv 8
                type solution status = solved
                applicability = applicable
                verification = failure
model:
state: fpcr = #x0000000000000000
state: clif_load = {active: false, size_bits: 0, addr: #x0000000000000000}
state: isa_store = {active: false, size_bits: 1, addr: #x0000000000000000, value: #x0000000000000000}
state: loaded_value = #x0000000000000000
state: clif_store = {active: false, size_bits: 1, addr: #x0000000000000000, value: #x0000000000000000}
state: exec_trap = false
state: clif_trap = true
state: isa_load = {active: false, size_bits: 0, addr: #x0000000000000000}
has_type({bits: 8}, #x00) -> #x00
fits_in_64({bits: 8}) -> {bits: 8}
sdiv({bits: 0}, #x80, #xff) -> #x00
...
MInst.Extend(#xffffffffffffffff, #x00000000000000ff, true, #x08, #x40) -> {flags_in: {N: #b0, Z: #b0, C: #b0, V: #b0}, flags_out: {N: #b0, Z: #b0, C: #b0, V: #b0}}
emit({flags_in: {N: #b0, Z: #b0, C: #b0, V: #b0}, flags_out: {N: #b0, Z: #b0, C: #b0, V: #b0}}) -> #b1
lower(#x00) -> #x80
Error: verify expansion: 416

Caused by:
    verification failed
```

Note that the state variables `clif_trap` and `exec_trap` tell us that this
counterexample would trap in CLIF (Cranelift Intermediate Representation) semantics, but not in the lowered
(ISA) instructions. The inputs to the `sdiv` term also show us concrete inputs that
trigger the bug.

Once you have examined the failed verification output, reapply the fix (now without the `-R` flag):

```
patch -p1 -d /root/arrival/ -i /root/artifact/data/sdiv_fix.patch
```

Then rerun verification against the `sdiv` rule:
```
./script/veri.sh -- --filter rule:sdiv
```

You should now see verification succeed.

### Verify AArch64 Instruction Selection Rules [est. 15-60 mins]

#### Verification Run

Within the container, invoke the evaluation verification run (in CI mode, e.g., with a shorter 60 second timeout for each verification query):
```
./script/verify/eval.sh -n artifact -c -t 60
```

The options provided to the evaluation script specify the name `artifact` for
the verification run (`-n artifact`), enable CI mode (`-c`) and use a timeout of
60 seconds (`-t 60`).

This command will take a while to run. More cores will help, since Arrival
processes expansions in parallel. On the x86 machines we tested, this took between 10 and 60 minutes.

While this is running you should see log output as it processes expansions, for example:
```
...
[2025-06-23T21:34:01Z INFO ] #1857      ishl_64
[2025-06-23T21:34:01Z INFO ] #2043      icmp
[2025-06-23T21:34:01Z INFO ] #1903      sshr_64
[2025-06-23T21:34:01Z INFO ] #1950      clz_16
[2025-06-23T21:34:01Z INFO ] #1486      sextend_load
[2025-06-23T21:34:01Z INFO ] #650       uextend_load
...
```

A complete dump of results will be written under the `$EVAL_DATA_DIR/run`
directory mounted in the container (`data/eval/run` of the artifact).
Specifically, it will be written to a directory with a Run ID of the form
`<timestamp>-artifact`, where `artifact` comes from the name option (`-n`)
provided above.  Locate the run directory and its ID with:
```
ls -1d $EVAL_DATA_DIR/run/*-artifact
```

Your Run ID should look something like `2025-06-24T03:02:28-artifact`.

#### Analysis

Below we will see how to fully analyze verification runs to produce the paper's
results, but for now lets see how to post-process this verification run alone.

In the container, change into the evaluation directory:
```
cd /root/artifact/eval/
```

Take the Run ID just generated and run:
```
./process.py --run-id <run id> coverage
```

This will produce tabular data in the same format as Table 1 in the paper, for
example the output may look similar to:
```
\begin{tabular}{rrrrrrrr}
\toprule
\multicolumn{3}{c}{} & \multicolumn{3}{c}{Result} & \multicolumn{2}{c}{Solver} \\
\cmidrule(lr){4-6} \cmidrule(lr){7-8}
Category & Expansions & Type Inst. & Verified & Timeout & Inapplicable & cvc5 & Z3 \\
\midrule
Memory & 5,832 & 10,368 & 10,368 & 0 & 0 & 10,368 & 0 \\
Float & 97 & 85 & 56 & 10 & 19 & 85 & 0 \\
Rest (Integer etc.) & 283 & 563 & 355 & 45 & 163 & 563 & 0 \\
\midrule
Total & 6,212 & 11,016 & 10,779 & 55 & 182 & 11,016 & 0 \\
\bottomrule
\end{tabular}
```

You can pass the `--ascii` flag to format as an ASCII table rather than LaTeX:
```
./process.py --ascii --run-id <run id> coverage
```

The numbers you see should be as in Table 1 with the expected differences
between the CI run and full verification run:

* More timeouts, and therefore also fewer total verified, because of the 60 second timeout.
* Zero expansions run under Z3. CI mode uses CVC5 only.

### Generate ISA Specifications [est. 10 mins]

Run the container:
```
./script/docker_run.sh
```

Generated specifications are checked in to the Cranelift codebase. Within the container, you can
locate them with:
```
grep -lR 'GENERATED BY `isaspec`' /root/arrival/cranelift/codegen/src/isa/aarch64/spec/
```

To confirm the code generation works from scratch, first delete the generated files:
```
grep -lR 'GENERATED BY `isaspec`' /root/arrival/cranelift/codegen/src/isa/aarch64/spec/ | xargs rm -v
```

Then, rerun the specification generation using the following script:
```
cd ../isaspec
./script/generate.sh -l
```

This will build the ISA specification generation tooling, launch a server to
communicate with SymASLp symbolic executor, and run the code generator on
configured instruction families. You should see log output like the following
(trimmed for brevity):
```
Disassembling '0x8b0600a4'
Disassembling '0xcb0600a4'
Disassembling '0xaa0600a4'
...
Disassembling '0x9ba61ca4'
Disassembling '0x9b261ca4'
Disassembling '0x244:10|bits:12|0xa4:10'
Disassembling '0x245:10|bits:12|0xa4:10'
Disassembling '0x344:10|bits:12|0xa4:10'
Disassembling '0x345:10|bits:12|0xa4:10'
Disassembling '0x2c4:10|bits:12|0xa4:10'
Disassembling '0x2c5:10|bits:12|0xa4:10'
Disassembling '0x3c4:10|bits:12|0xa4:10'
Disassembling '0x3c5:10|bits:12|0xa4:10'
Disassembling '0x44:10|bits:12|0xa4:10'
Disassembling '0x45:10|bits:12|0xa4:10'
Disassembling '0x144:10|bits:12|0xa4:10'
Disassembling '0x145:10|bits:12|0xa4:10'
Disassembling '0xc4:10|bits:12|0xa4:10'
Disassembling '0xc5:10|bits:12|0xa4:10'
Disassembling '0x0e71b8a4'
...
Disassembling '0x6e31a8a4'
Disassembling '0x4e31b8a4'
Disassembling '0x2e31a8a4'
Disassembling '0x0e31b8a4'
```

Upon completion, you should see that the ISA specification ISLE files have been
regenerated, as before:
```
grep -lR 'GENERATED BY `isaspec`' /root/arrival/cranelift/codegen/src/isa/aarch64/spec/
```

Take a look at some of the generated files to see the produces specifications. For example,
the follow is the specification for a two register, one immediate (`rri`) floating-point (`fpu`)
instruction:

```
cat /root/arrival/cranelift/codegen/src/isa/aarch64/spec/fpu_rri.isle
```

### Run Evaluation Analysis [est. 5 mins]

In this section we show how to run the analysis that produces the major tables,
figures, and statistics in the paper.

The main inputs to the analysis are results from two verification runs, as
produced by the the `eval.sh` script above. Specifically, we ran both a "full"
verification run with a long timeout, and a "CI" run with a 60 second timeout
and CVC5 only. In the above, we showed how to execute a CI run, which completes
in a reasonable amount of time.  Since the full run is very time consuming, we
instead provide the cached verification run outputs that were used for the paper
evaluation.  All evaluation run outputs may be seen in the `data/eval/run`
directory of the artifact.

From inside the container, change into the evaluation directory:
```
cd /root/artifact/eval/
```

Generate evaluation artifacts by running `make`:
```
make
```

Results should be written to the `generated` sub-directory:
```
ls generated/
```

Specifically:

* `coverage.tex`: Table 1. Verification results for selected Wasm-critical subset.
* `specs.tex`: Table 2. Specifications required for the verification. 
* `timings.pdf`: Figure 5. Cumulative distribution graph of verification times.
* `stats.tex`: Computed statistics used in the paper, as LaTeX macros.
* `slow.tsv`: Expansions that were slow to verify.
* `timeouts.tsv`: Expansions that timed out.
* `terms.tsv`: Terms involved in the verification and their metadata.

### Optional: Full Verification Runs [est. 12+ hours]

#### Verification Runs

To execute a full verification run with 4 hour timeout:
```
./script/verify/eval.sh -n full -t 14400
```

For the CI verification run, either reuse results from above, or run with:
```
./script/verify/eval.sh -n ci -c -t 60
```

As above, look for the Run IDs under the `$EVAL_DATA_DIR/run` directory.

#### Analysis

The evaluation analysis is the same as above, but we'll swap out the Run IDs to
point at the new data collected.

From inside the container, change into the evaluation directory:
```
cd /root/artifact/eval/
```

Edit the `overrides.args` arguments file to point the evaluation at the new Run
IDs. It should look something like the following:
```
--run-id
2025-07-01T05:32:13-full
--ci-run-id
2025-07-01T18:35:53-ci
```

Generate evaluation artifacts by running `make`, which will pickup the arguments from `overrides.args`:
```
make
```

Results should be written to the `generated` sub-directory:
```
ls generated/
```

Inspect the generated files as described in the evaluation analysis section
above. Small differences from the pre-collected data are likely given the
difference in hardware, in particular you may see different rules timing out.

## Reusability Guide

Our core reusable artifact is the Arrival verifier.  Arrival is open-source as
part of our fork of Wasmtime/Cranelift at
[mmcloughlin/arrival](https://github.com/mmcloughlin/arrival/tree/oopsla25-artifact).
For documentation on how to use the Arrival verifier:

* [Getting Started Guide](https://github.com/mmcloughlin/arrival/blob/oopsla25-artifact/cranelift/isle/veri/README.md)
* [Specification Language Reference](https://github.com/mmcloughlin/arrival/blob/oopsla25-artifact/cranelift/isle/veri/docs/language.md)

Our SymASLp fork of ASLp is public at
[mmcloughlin/aslp](https://github.com/mmcloughlin/aslp).  The integration of
Arrival with ASLp is part of our core reusable artifact in the Rust crate
[cranelift/isle/veri/isaspec](https://github.com/mmcloughlin/arrival/tree/oopsla25-artifact/cranelift/isle/veri/isaspec).
