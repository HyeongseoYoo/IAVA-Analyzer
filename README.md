# IAIA-Analyzer

**Interrupt-Aware Index Analysis Tool for SSD Firmware**

A static analyzer for detecting interrupt-induced out-of-bounds array accesses in SSD firmware models.

The analyzer defines an interrupt-aware abstract semantics for a small imperative language (*.si) and implements it as an abstract interpreter. It over-approximates possible interrupt effects by applying a handler fixpoint during analysis.

In addition to abstract values, the analyzer tracks provenance information, recording where each value may have been assigned. Using this provenance, it reports array indices that may become unsafe due to interrupt handler updates.

## How it works

Programs are written in a small imperative language with:

* `init { ... handler N { ... } ... }` — global initialization and interrupt handlers
* `main { ... }` — the main thread
* Heap arrays (`malloc`, `*arr[idx]`), scalars, `if/then/else`, and `while/do`
* `disable` / `enable` — control interrupt delivery

The analyzer performs interval-domain abstract interpretation. At selected yield
points in `main`, it joins the current abstract state with the fixpoint effect of
the interrupt handlers. This over-approximates values that may be changed by
interrupts during execution.

The analyzer also tracks provenance information for abstract values. When an
array index may go out of range, the provenance is used to check whether the
unsafe value may come from a handler write. Such cases are reported as
interrupt-related warnings.

## Build

Requires OCaml ≥ 5.3.0 and Menhir.

```bash
opam install menhir
dune build
```

## Usage

```bash
./_build/default/bin/main.exe [option] <file.si>
```

| Option             | Description                                                   |
| ------------------ | ------------------------------------------------------------- |
| `-pp`              | Pretty-print the labeled program                              |
| `-tab`             | Print the label table                                         |
| `-dintp`           | Run the concrete interpreter                                  |
| `-analyze`         | Run the abstract analyzer                                     |
| `-analyzedetail`   | Run the abstract analyzer with full state output              |
| `-summary`         | Print pre-compiled handler summaries                          |
| `-prov`            | Run provenance analysis and report interrupt-related warnings |
| `-report`          | Generate a Markdown bug report using Codex                    |
| `-report-out FILE` | Write the `-report` output to `FILE`                          |
| `-optoff`          | Disable the handler-fixpoint optimization                     |

### Example

```bash
# Report interrupt-related OOB warnings with handler provenance
./_build/default/bin/main.exe -prov examples/bench9_nvme_bug.si

# Run the same analysis without the compiled-fixpoint optimization
./_build/default/bin/main.exe -optoff -prov examples/bench9_nvme_bug.si
```

## Language quick reference

```
// init block: global state and handlers
init {
  BUF_SZ := 16;
  Buf    := malloc(16, 0);   // heap array of 16 zeros
  Idx    := 0;

  handler 0 {
    Idx := 255              // may change Idx between a check and its use
  }
}

// main block
main {
  while Idx < BUF_SZ do (
    *Buf[Idx] := 0;         // OOB if handler 0 fires after the while-check
    Idx := Idx + 1
  )
}
```

## Benchmarks

The main evaluation inputs are in `benchmark/`. They are organized as a clean
benchmark suite with expected analysis results.

| Directory    | Contents                                                           |
| ------------ | ------------------------------------------------------------------ |
| `benchmark/` | Micro and composite benchmark suite with expected warnings         |
| `examples/`  | Miscellaneous development examples used while testing the analyzer |

### Benchmark suite (`benchmark/`)

Seven **micro benchmarks** isolate small interrupt-related patterns. They share
the same `init` block to make the differences easier to compare.

| File                         | Pattern                | Expected                     |
| ---------------------------- | ---------------------- | ---------------------------- |
| `micro1_direct_buggy.si`     | Direct shared scalar   | 1 interrupt-related warning  |
| `micro1_direct_fixed.si`     | Direct shared scalar   | 0 warnings                   |
| `micro2_alias1_buggy.si`     | 1-depth pointer alias  | 1 interrupt-related warning  |
| `micro2_alias1_fixed.si`     | 1-depth pointer alias  | 0 warnings                   |
| `micro3_aliasmulti_buggy.si` | Multi-depth heap alias | 1 interrupt-related warning  |
| `micro3_aliasmulti_fixed.si` | Multi-depth heap alias | 0 warnings                   |
| `micro4_nonhandler_oob.si`   | Non-handler OOB case   | 0 interrupt-related warnings |

Two **composite benchmarks** combine several patterns in larger HIL-inspired
programs.

| File                      | Theme                       | Expected                                         |
| ------------------------- | --------------------------- | ------------------------------------------------ |
| `composite1_nvme_sqcq.si` | NVMe SQ/CQ command pipeline | 3 interrupt-related warnings + 1 non-handler OOB |
| `composite2_uecc_dma.si`  | UECC error recovery and DMA | 3 interrupt-related warnings + 1 non-handler OOB |

See `benchmark/README.md` for per-bug line numbers and expected OOB ranges.


## Project structure

```text
lib/
  syntax.ml       language AST
  lexer.mll       lexer
  parser.mly      Menhir grammar
  interp.ml       concrete interpreter
  itv.ml          interval domain
  abs_dom.ml      abstract domain
  analyzer.ml     abstract interpreter and handler fixpoint
  provenance.ml   provenance tracking and warning classification
  reporter.ml     Codex-based Markdown report generation
bin/
  main.ml         CLI entry point
benchmark/        micro and composite benchmark 
examples/         testing examples
suite
```

## Author

Hyeongseo Yoo — Seoul National University & Samsung Electronics ([hsyoo@ropas.snu.ac.kr](mailto:hsyoo@ropas.snu.ac.kr))
