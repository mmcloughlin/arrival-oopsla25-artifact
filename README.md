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

> In the Hardware Dependencies section, describe the hardware required to evaluate
> the artifact. If the artifact requires specific hardware (e.g., many cores, disk
> space, GPUs, specific processors), please provide instructions on how to gain
> access to the hardware. Keep in mind that reviewers must remain anonymous.

TODO

## Getting Started Guide

### Setup [est. 10-30 minutes]

We provide our artifact as a
[Docker](https://docs.docker.com/engine/installation/) instance. Please install
Docker based on your system's instructions.

Build the `arrival` Docker image from the root of the artifact as follows:

```
docker build -t arrival .
```

### Basic Test [est. 5 minutes]

Run the container:
```
docker run -it arrival
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

TODO

## Reusability Guide

> In the Reusability Guide, explain which parts of your artifact constitute the
> core pieces which should be evaluated for reusability. Explain how to adapt the
> artifact to new inputs or new use cases. Provide instructions for how to
> find/generate/read documentation about the core artifact. Articulate any
> limitations to the artifact’s reusability.

TODO
