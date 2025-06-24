# Artifact Evaluation: Scaling Instruction-Selection Verification with Authoritative ISA Semantics in an Industrial WebAssembly Compiler

## Introduction

> In the Introduction, briefly explain the purpose of the artifact and how it
> supports the paper. We recommend listing all claims in the paper and stating
> whether or not each is supported. For supported claims, say how the artifact
> provides support. For unsupported claims, explain why they are omitted.

Artifact for our OOPSLA 2025 paper "Scaling Instruction-Selection Verification
with Authoritative ISA Semantics in an Industrial WebAssembly Compiler".

TODO

## Hardware Dependencies

This artifact requires a single x86 host machine. While there is no strict
requirement for many cores, the Arrival verifier parallelizes over available
cores, and reviewers will have a better time with at least 8 and ideally more
cores.

## Getting Started Guide

### Setup [est. 10-30 minutes]

We provide our artifact as a
[Docker](https://docs.docker.com/engine/installation/) instance. Please install
Docker based on your system's instructions.

Build the `arrival` Docker image from the root of the artifact as follows:

```
./script/docker_build.sh
```

### Basic Test [est. 5 minutes]

Run the container:
```
./script/docker_run.sh
```

Verify a simple lowering of the integer addition `iadd` instruction, execute
within the container:
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

> In the Step by Step Instructions, explain how to reproduce any experiments or
> other activities that support the conclusions in your paper. Write this for
> readers who have a deep interest in your work and are studying it to improve it
> or compare against it. If your artifact runs for more than a few minutes, point
> this out, note how long it is expected to run (roughly) and explain how to run
> it on smaller inputs. Reviewers may choose to run on smaller inputs or larger
> inputs depending on available resources.
>
> Be sure to explain the expected outputs produced by the Step by Step
> Instructions. State where to find the outputs and how to interpret them relative
> to the paper. If there are any expected warnings or error messages, explain
> those as well. Ideally, artifacts should include sample outputs and logs for
> comparison.

### Reproduce `sdiv` Bug [est. 5 mins]

To reproduce the `sdiv` bug discussed in Section 2.4 "Bug Discovery with
Arrival", first run the docker container:

```
./script/docker_run.sh
```

Since the bug has now been fixed, revert the change to reintroduce the error:
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
counterexample would trap in CLIF semantics, but not in the lowered
instructions. The inputs to the `sdiv` term also show us concrete inputs that
trigger the bug.

### Verify AArch64 Instruction Selection Rules [est. 15-60 mins]

#### Verification Run

Run the container:
```
./script/docker_run.sh
```

Within the container, invoke the evaluation verification run (in CI mode):
```
./script/verify/eval.sh -n artifact -c -t 60
```

The options provided to the evaluation script specify the name `artifact` for
the verification run (`-n artifact`), enable CI mode (`-c`) and use a timeout of
60 seconds (`-t 60`).

This command will take a while to run. More cores will help, since Arrival
processes expansions in parallel.

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

The numbers you see should be as in Table 1 with the expected differences
between the CI run and full verification run:

* More timeouts, and therefore also fewer total verified, because of the 60 second timeout.
* Zero expansions run under Z3. CI mode uses CVC5 only.

### Generate ISA Specifications [est. 10 mins]

Run the container:
```
./script/docker_run.sh
```

Generated specifications are checked in to the Cranelift codebase. You can
locate them with:
```
grep -lR 'GENERATED BY `isaspec`' /root/arrival/cranelift/codegen/src/isa/aarch64/spec/
```

To confirm the code generation works from scratch, first delete the generated files:
```
grep -lR 'GENERATED BY `isaspec`' /root/arrival/cranelift/codegen/src/isa/aarch64/spec/ | xargs rm -v
```

Generate them using the following script:
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

To run the evaluation data analysis, first run the container:
```
./script/docker_run.sh
```

Change into the evaluation directory:
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
* `specs.tex`: Table 2. Specifications required for
* `timings.pdf`: Figure 5. Cumulative distribution graph of verification times.
* `stats.tex`: Computed statistics used in the paper, as LaTeX macros.
* `slow.tsv`: Expansions that were slow to verify.
* `timeouts.tsv`: Expansions that timed out.
* `terms.tsv`: Terms involved in the verification and their metadata.

## Reusability Guide

> In the Reusability Guide, explain which parts of your artifact constitute the
> core pieces which should be evaluated for reusability. Explain how to adapt the
> artifact to new inputs or new use cases. Provide instructions for how to
> find/generate/read documentation about the core artifact. Articulate any
> limitations to the artifact’s reusability.

TODO
