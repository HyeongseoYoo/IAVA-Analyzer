# Interrupt-Aware Index-Analysis Benchmark Suite

A clean benchmark suite for testing interrupt-aware out-of-bounds array analysis.
Every file targets **exactly one concern** to make pass/fail straightforward.

---

## Shared micro init (byte-identical)

All seven micro files share the same `init { ... }` block (verified by MD5).
Only the `main` block differs. The shared init declares:

| Constant | Value | Role |
|---|---|---|
| `MAX_SLOTS` | 8 | SqBuf, PrpList, PrpList2, TxBuf sizes |
| `PRP_SZ` | 8 | valid index range for PrpList/PrpList2 |
| `TX_TOTAL` | 10 | loop bound for micro4 (> MAX_SLOTS → OOB) |
| `ABORT_SLOT` | 255 | right-OOB sentinel written by handlers 1 and 2 |
| `LEFT_ABORT_SLOT` | -1 | left-OOB sentinel written by handler 0 |
| `REG_PRPPTR` | 1 | field index written/read through aliases |

Handler assignments:

| Handler | Event | Effect | Bug source for |
|---|---|---|---|
| 0 | `NVME_ERR_EVT` | `SlotIdx := LEFT_ABORT_SLOT` | micro1 |
| 1 | `TIMEOUT_EVT` | `*RegFile[REG_PRPPTR] := ABORT_SLOT` | micro2 |
| 2 | `CQ_FULL_EVT` | `DmaPtr := DmaConf; *DmaPtr[REG_PRPPTR] := ABORT_SLOT` | micro3 |

No handler writes `TxIdx`, `TxBuf`, or `DiagBuf`, so micro4's OOB is non-handler-caused.

---

## Micro benchmarks

### micro1_direct_buggy.si / micro1_direct_fixed.si — Pattern 1: direct scalar

**Bug site:** `*SqBuf[SlotIdx]` (line 74 of buggy file)

`SlotIdx` is checked against both bounds (`LEFT_ABORT_SLOT < SlotIdx` and
`SlotIdx < MAX_SLOTS`), but handler 0 can fire at the yield point between the
inner check and the `then`-branch body, overwriting `SlotIdx` with
`LEFT_ABORT_SLOT` (-1) before the `SqBuf` write. This benchmark is the suite's
left-OOB micro case.

| | Expected |
|---|---|
| Buggy | 1 warning — `interrupt influence: handler 0`, left OOB `[-1, -1]` |
| Fixed | 0 warnings |

**Fix strategy:** wrap the bounds checks and write in `disable/enable`, and
check both sides of the interval:
```
disable;
if SlotIdx < MAX_SLOTS
then (
  if LEFT_ABORT_SLOT < SlotIdx then *SqBuf[SlotIdx] := ERR_NONE else unit
)
else unit;
enable
```
Inside the critical section no handler can fire, so the checks on `SlotIdx`
are stable and the write is provably safe.

---

### micro2_alias1_buggy.si / micro2_alias1_fixed.si — Pattern 2: 1-depth alias

**Bug site:** `*PrpList[*NvmeCtrl[REG_PRPPTR]]` (line 72 of buggy file)

`NvmeCtrl := RegFile` creates a variable alias to the same heap block that
handler 1 corrupts (`*RegFile[REG_PRPPTR] := ABORT_SLOT`).  The `while`
condition checks `*NvmeCtrl[REG_PRPPTR] < PRP_SZ`, but between that check
and the loop body, handler 1 can fire and store 255 into `RegFile[REG_PRPPTR]`.
The second (unguarded) read in the body then uses 255 as an index into
`PrpList[8]`.  Because the loop also increments `REG_PRPPTR`, the abstract
value is unbounded; the analyzer reports right OOB `[8, ∞]`.

| | Expected |
|---|---|
| Buggy | 1 warning — `interrupt influence: handler 1`, right OOB `[8, ∞]` |
| Fixed | 0 warnings |

**Fix strategy:** inside the loop body, capture the value atomically before use:
```
disable;
SafePrp := *NvmeCtrl[REG_PRPPTR];
enable;
if SafePrp < PRP_SZ then *PrpList[SafePrp] := 0 else unit
```
`SafePrp` is a plain variable no handler writes, so the `if`-check narrowing
survives into the `then`-branch.

---

### micro3_aliasmulti_buggy.si / micro3_aliasmulti_fixed.si — Pattern 3: multi-depth heap alias

**Bug site:** `*PrpList2[*EngRef[REG_PRPPTR]]` (line 79 of buggy file)

Main builds a 3-level access chain to `DmaConf[REG_PRPPTR]`:
1. `SysRef := DescTable` — var alias to the descriptor table
2. `EngRef := *SysRef[0]` — heap dereference; `EngRef` now aliases `DmaConf`
3. `*EngRef[REG_PRPPTR]` — reads `DmaConf[REG_PRPPTR]`

Handler 2 reaches the same cell through a 2-level path (`DmaPtr := DmaConf;
*DmaPtr[REG_PRPPTR] := ABORT_SLOT`).  The aliasing is hidden from main's
perspective: the chain goes through a heap-stored pointer (`DescTable[0]`),
not a visible variable assignment.  Handler 2 can fire between the `if`-check
and the `then`-branch, corrupting `DmaConf[REG_PRPPTR]` to 255.

| | Expected |
|---|---|
| Buggy | 1 warning — `interrupt influence: handler 2`, right OOB `[8, 255]` |
| Fixed | 0 warnings |

**Fix strategy:** snapshot via `disable; SafePrp2 := *EngRef[REG_PRPPTR]; enable`
then use `SafePrp2` (not re-read through the alias) for the bounds check and index.

---

### micro4_nonhandler_oob.si — Precision test (no fixed version)

**Bug site:** `*TxBuf[TxIdx]` (line 72)

`TX_TOTAL` (10) > `MAX_SLOTS` (8).  The loop runs `TxIdx` from 0 to 9; iterations
at `TxIdx` ∈ {8, 9} access `TxBuf` out of bounds.  No handler writes `TxIdx` or
`TxBuf`, so the error is entirely in `main`.

| | Expected |
|---|---|
| Only version | right OOB `[8, 9]` internally, but **0 warnings** under `-prov` (non-handler-caused, filtered out of the report entirely) |

This is a **precision test**: the analyzer should correctly attribute the OOB to
main alone, not to any handler.

---

## Composite benchmarks

The buggy composites contain all three interrupt bug patterns and one
non-handler OOB in a single realistic scenario, testing detection and precision
together. Each has a matching `_fixed` version that should report no
interrupt-caused warnings under `-prov`.
Line counts: composite1 961/957 buggy/fixed, composite2 966/965 buggy/fixed.

---

### composite1_nvme_sqcq.si / composite1_nvme_sqcq_fixed.si — NVMe SQ/CQ Command Processing Pipeline

Models a full NVMe HIL pipeline: capability negotiation → namespace enumeration →
queue init → PRP list construction → 5-opcode command dispatch → CQ entry
staging → CQ drain → error/abort handling → LBA validation → ring reconciliation →
event-log aggregation → NS info page → feature-set update → PRP consistency check.
The base file is the buggy version; `_fixed` preserves the same scenario but
closes the three interrupt-caused windows.

**Phase map:**

| Phase | Lines | Description |
|---|---|---|
| 0 | ~185–208 | Controller capability and feature setup |
| 1 | ~209–239 | Namespace enumeration and LBA map init |
| 2 | ~240–259 | SQ/CQ ring and CQ-entry buffer init |
| 3 | ~260–279 | PRP page list and DMA descriptor construction |
| 4 | ~280–303 | Command pre-load (READ×4, WRITE×2, FLUSH, IDENTIFY) |
| 5 | ~304–460 | **Command dispatch — BUGS HERE** (interrupts enabled) |
| 6 | ~461–483 | Data buffer preparation from LBA map + PRP traversal |
| 7 | ~484–519 | CQ entry staging and ring write with phase-tag |
| 8 | ~520–560 | CQ drain and host doorbell advance |
| 9 | ~561–645 | Error classification and abort-list processing |
| 10 | ~646–687 | LBA address map validation (per-NS capacity check) |
| 11 | ~688–720 | CQ phase-tag management and ring wrap |
| 12 | ~721–768 | SQ ring pointer reconciliation |
| 13 | ~769–793 | Event log aggregation and error statistics |
| 14 | ~794–840 | Abort-list resolution and command state finalization |
| 15 | ~841–860 | Namespace information page construction |
| 16 | ~861–886 | Feature-set response and NvmeRegs update |
| 17 | ~887–957 | PrpList consistency check |
| 18 | ~958–961 | **True-negative statistics scan** |

**Ground truth:**

| Bug | Site | Line | Phase | Handler | OOB range | Pattern |
|---|---|---|---|---|---|---|
| Bug-1 | `*SqBuf[CmdSlot]` | 325 | 5 | 0 (NVME_ERR_EVT) | `[8, 255]` | direct scalar |
| Bug-2 | `*PrpList[*NvmeCtrl[REG_PRPPTR]]` | 448 | 5 | 1 (TIMEOUT_EVT) | `[8, 255]` | 1-depth alias |
| Bug-3 | `*PrpList2[*EngRef[REG_PRPPTR]]` | 455 | 5 | 2 (CQ_FULL_EVT) | `[8, 255]` | multi-depth alias |
| True-neg | `*StatBuf[StatIdx]` | 958 | 18 | none | `[8, 8]` | main-only OOB |

All phases except Phase 5 are wrapped in `disable/enable` to confine
handler-caused OOBs to exactly the three documented bugs.  The `SafeSlot`
capture pattern inside Phase 5 protects all other array accesses in each
dispatch iteration.

**Expected for the buggy file:** 3 warnings under `-prov`; Bug-1/2/3 show
`interrupt influence: handler N`. The true-negative is still an internal OOB,
but it is filtered out of the provenance report.

**Fixed version:** `composite1_nvme_sqcq_fixed.si` re-checks `CmdSlot` inside
`disable/enable`, snapshots `NvmeRegs[REG_PRPPTR]` into `SafePrp`, and snapshots
`DmaConf[REG_PRPPTR]` into `SafePrp2`. Expected under `-prov`: 0 warnings.

---

### composite2_uecc_dma.si / composite2_uecc_dma_fixed.si — UECC Error Detection + DMA PRP-List Pipeline

Models a full UECC/DMA error-recovery pipeline: ECC engine init → channel/plane
preflight → page state init → DMA descriptor construction → read-request issue →
UECC correction (5-syndrome dispatch) → DMA PRP-list traversal → retry queue →
channel recovery → power-state management → syndrome re-scan → error aggregation
→ link snapshot → page reconciliation → channel utilization → plane finalization
→ ECC histogram → DMA teardown → error budget check → ECC engine re-arm.

`FATAL_SLOT = 200` deliberately differs from the micro suite's 255, demonstrating
that OOB ranges follow the handler's sentinel value.
The base file is the buggy version; `_fixed` preserves the same scenario but
closes the three interrupt-caused windows.

**Phase map:**

| Phase | Lines | Description |
|---|---|---|
| 0 | ~184–207 | ECC engine initialisation and syndrome table zero |
| 1 | ~208–246 | Channel and plane preflight (LINK_UP check) |
| 2 | ~247–261 | Page state and error table initialisation |
| 3 | ~262–287 | DMA descriptor table construction |
| 4 | ~288–322 | Flash page read-request construction |
| 5 | ~323–441 | **UECC correction loop — BUGS HERE** (interrupts enabled) |
| 6 | ~442–470 | DMA PRP-list traversal |
| 7 | ~471–505 | Retry queue processing |
| 8 | ~506–541 | Channel state recovery |
| 9 | ~542–572 | Power state management |
| 10 | ~573–601 | Syndrome table re-scan and ErrTable finalization |
| 11 | ~602–641 | Error log aggregation |
| 12 | ~642–662 | Link table snapshot and soft-reset |
| 13 | ~663–701 | Per-page state reconciliation |
| 14 | ~702–733 | Channel utilization accounting |
| 15 | ~734–775 | Plane state finalization |
| 16 | ~776–819 | ECC syndrome histogram |
| 17 | ~820–843 | DMA descriptor teardown |
| 18 | ~844–901 | Error budget threshold check |
| 19 | ~902–960 | ECC engine re-arm and pending-list reset |
| 20 | ~961–966 | **True-negative statistics scan** |

**Ground truth:**

| Bug | Site | Line | Phase | Handler | OOB range | Pattern |
|---|---|---|---|---|---|---|
| Bug-1 | `*ErrTable[ErrSlot]` | 339 | 5 | 0 (UECC_EVT) | `[8, 200]` | direct scalar |
| Bug-2 | `*PendList[*ErrCtrl[REG_PNDPTR]]` | 426 | 5 | 1 (DMA_TIMEOUT) | `[8, 200]` | 1-depth alias |
| Bug-3 | `*PendList2[*EngPtr[REG_PNDPTR]]` | 433 | 5 | 2 (RETRY_EVT) | `[8, 200]` | multi-depth alias |
| True-neg | `*RetryLog[RetryIdx]` | 963 | 20 | none | `[8, 8]` | main-only OOB |

**Design note — safe accesses in Phase 6:**
`*DataBuf[*DmaDescBuf[DmaIdx]]` used to be a double heap-read; the if-check on
the re-read heap cell cannot narrow a heap value, only a variable.  The fix
captures the descriptor value into `DmaPage` first, then guards `DmaPage` — a
plain variable no handler writes — which the if-check can narrow stably.

**Expected for the buggy file:** 3 warnings under `-prov`; Bug-1/2/3 show
`interrupt influence: handler N`. The true-negative is still an internal OOB,
but it is filtered out of the provenance report.

**Fixed version:** `composite2_uecc_dma_fixed.si` re-checks `ErrSlot` inside
`disable/enable`, snapshots `ErrRegs[REG_PNDPTR]` into `SafePend`, and snapshots
`EccConf[REG_PNDPTR]` into `SafePend2`. Expected under `-prov`: 0 warnings.

---

## STM32 benchmarks (real-firmware translations + injected bugs)

Four files derived from official STM32CubeF0 HAL examples (see
`benchmark/stm32_conversion_prompt.md` for the conversion methodology).
Each file models one *task* instance of a larger always-on firmware
(SSD-style: an infinite loop dispatching queued tasks) rather than the
literal `while(1)` forever-loop — consistent with how every other
benchmark in this suite is a single bounded task execution, not the loop
itself.

A faithful translation of each official example is, on its own, a true
negative — the real ST example code has no main/ISR array-index race for
the analyzer to find. To give the analyzer a real target, each file has
one or more synthetic main tasks and matching ISR-side corruptions added
on top of the faithful translation, clearly marked `[INJECTED]` in the
source. `stm32_gpio_exti.si` and `stm32_tim_timebase.si` are the shorter
translated examples and each carry exactly **one** injected bug.
`stm32_dma_flashtoram.si` and `stm32_uart_twoboards_comit.si` are longer
and each carry **two**: one interrupt-race pattern plus one non-handler
precision test, mirroring how the composite benchmarks above combine
multiple patterns in one realistic scenario. Across all four files, every
one of the suite's four bug patterns appears at least once.

| File | Real example | Bugs | Patterns | Expected `-prov` |
|---|---|---|---|---|
| `stm32_gpio_exti.si` | GPIO/GPIO_EXTI | 1 | 1: direct scalar | 1 warning, left OOB |
| `stm32_tim_timebase.si` | TIM/TIM_TimeBase | 1 | 2: 1-depth alias | 1 warning, right OOB |
| `stm32_dma_flashtoram.si` | DMA/DMA_FLASHToRAM | 2 | 2: 1-depth alias + 4: precision test | 1 warning, right OOB (+1 filtered candidate) |
| `stm32_uart_twoboards_comit.si` | UART/UART_TwoBoards_ComIT | 2 | 3: multi-depth heap alias + 4: precision test | 1 warning, right OOB (+1 filtered candidate) |

### stm32_gpio_exti.si — Pattern 1: direct scalar

Mirrors `micro1_direct_buggy.si`. Task role: per-line EXTI event-count
task. EXTI0_1_IRQHandler is, by name, a shared vector for two physical
lines (EXTI0/EXTI1); this model only wires up line 0, but the injected
main task keeps a small per-line event counter table (`PinEventTable`)
indexed by `GpioCallback_Pin` (the dispatch pin recorded by handler 0).
Handler 0 (faithfully toggling LED3) also overwrites `GpioCallback_Pin`
with the sentinel `EXTI_PIN_ABORT` (-1) on every firing — modeling the
"other" multiplexed line that this translation never wires up. Handler 0
can fire at the yield point between main's bounds checks and the
`PinEventTable` write.

**Bug site:** `*PinEventTable[GpioCallback_Pin]`

| | Expected |
|---|---|
| `-prov` | 1 warning — `interrupt influence: handler 0`, left OOB `[-1, -1]` |

### stm32_tim_timebase.si — Pattern 2: 1-depth alias

Mirrors `micro2_alias1_buggy.si`. Task role: command-timeout poll. Many
embedded/SSD firmwares scan a fixed-size pending-command table one slot
per main pass using a round-robin cursor that a periodic timer ISR also
drives forward. A new register field `TIM_REG_SLOT` on a small `TimRegs`
register file holds that cursor. The injected main task creates
`TimCtrl := TimRegs` (a 1-depth alias) and checks `*TimCtrl[TIM_REG_SLOT]`
before re-reading it as the index into `TimSlotTable`. Handler 0 (TIM2
period-elapsed, faithfully toggling LED5) can fire between the check and
the re-read, corrupting the same heap cell through the original
`TimRegs` name with the sentinel `TIM_SLOT_ABORT` (255).

**Bug site:** `*TimSlotTable[*TimCtrl[TIM_REG_SLOT]]`

| | Expected |
|---|---|
| `-prov` | 1 warning — `interrupt influence: handler 0`, right OOB `[8, 255]` |

### stm32_dma_flashtoram.si — Pattern 2 + Pattern 4

**Bug 1 (Pattern 2: 1-depth alias).** Task role: error-recovery salvage.
A new register field `DMA_REG_SALVAGE` on the existing `DmaChannel`
register file models a salvage cursor recorded on a transfer error. The
injected main task creates `DmaCtrl := DmaChannel` (a 1-depth alias) and
checks `*DmaCtrl[DMA_REG_SALVAGE]` before re-reading it as the index into
`aDST_Buffer`. Handler 1 (the faithfully modeled transfer-error path) can
fire between the check and the re-read, corrupting the same heap cell
through the original `DmaChannel` name with the sentinel
`DMA_SALVAGE_ABORT` (255).

**Bug site:** `*aDST_Buffer[*DmaCtrl[DMA_REG_SALVAGE]]`

| | Expected |
|---|---|
| `-prov` | 1 warning — `interrupt influence: handler 1`, right OOB `[32, 255]` |

**Bug 2 (Pattern 4: precision test, non-handler OOB).** Mirrors
`micro4_nonhandler_oob.si`. Task role: transfer-log sample-history.
`DMA_LOG_SAMPLES` (10) > `DMA_LOG_SIZE` (8); the injected main task fills
`DmaXferLog[DmaLogIdx]` for `DmaLogIdx` in `[0, 9]`, so `[8, 9]` is
out-of-bounds. `DmaLogIdx` is local to main and no handler ever writes it
or `DmaXferLog`, so this OOB is entirely main's own loop-bound mismatch.

**Bug site:** `*DmaXferLog[DmaLogIdx]`

| | Expected |
|---|---|
| `-prov` | right OOB `[8, 9]` internally, but **filtered out** (non-handler-caused) |

### stm32_uart_twoboards_comit.si — Pattern 3 + Pattern 4

**Bug 1 (Pattern 3: multi-depth heap alias).** Task role: command-slot
log. Firmware that dispatches/logs each TX/RX command cycle by a
rotating slot number is a common pattern; a new `UartCmdRegs[CMD_REG_SLOT]`
cell models that slot. The injected main task builds a 3-level access
chain entirely through a heap-stored pointer: `UartSysRef :=
UartDescTable; UartEngRef := *UartSysRef[0]`. Handler 1 (EXTI button
press, faithfully setting `UserButtonStatus`) reaches the same cell
through a separate 2-level path (`UartCmdPtr := UartCmdRegs`) and writes
the sentinel `UART_CMD_ABORT` (255) — an injected abort/cancel side
effect of the button press. The pre-existing, faithfully translated
`UartRxIdx`-guarded RX byte store is untouched and remains a true
negative (the handler-local bound is still sound).

**Bug site:** `*UartCmdLog[*UartEngRef[CMD_REG_SLOT]]`

| | Expected |
|---|---|
| `-prov` | 1 warning — `interrupt influence: handler 1`, right OOB `[4, 255]` |

**Bug 2 (Pattern 4: precision test, non-handler OOB).** Mirrors
`micro4_nonhandler_oob.si`. Task role: diagnostic sample-history.
`UART_DIAG_SAMPLES` (10) > `UART_DIAG_SIZE` (8); the injected main task
fills `UartDiagLog[UartDiagIdx]` for `UartDiagIdx` in `[0, 9]`, so
`[8, 9]` is out-of-bounds. `UartDiagIdx` is local to main and no handler
ever writes it or `UartDiagLog`, so this OOB is entirely main's own
loop-bound mismatch.

**Bug site:** `*UartDiagLog[UartDiagIdx]`

| | Expected |
|---|---|
| `-prov` | right OOB `[8, 9]` internally, but **filtered out** (non-handler-caused) |

---

## Verification

Run the correctness check with `-prov`:

```
./_build/default/bin/main.exe -prov benchmark/<file>.si
```

| File | Lines | OOB candidates | Interrupt warnings |
|---|---:|---:|---:|
| micro1_direct_buggy.si | 78 | 1 | 1 (handler 0, left OOB) |
| micro1_direct_fixed.si | 81 | 0 | 0 |
| micro2_alias1_buggy.si | 77 | 1 | 1 (handler 1) |
| micro2_alias1_fixed.si | 83 | 0 | 0 |
| micro3_aliasmulti_buggy.si | 82 | 1 | 1 (handler 2) |
| micro3_aliasmulti_fixed.si | 81 | 0 | 0 |
| micro4_nonhandler_oob.si | 76 | 1 | 0 (true-negative) |
| composite1_nvme_sqcq.si | 961 | 4 | 3 (one per handler) |
| composite1_nvme_sqcq_fixed.si | 957 | 1 | 0 (true-negative only) |
| composite2_uecc_dma.si | 966 | 4 | 3 (one per handler) |
| composite2_uecc_dma_fixed.si | 965 | 1 | 0 (true-negative only) |
| stm32_gpio_exti.si | 85 | 1 | 1 (handler 0, left OOB) |
| stm32_tim_timebase.si | 100 | 1 | 1 (handler 0, right OOB) |
| stm32_dma_flashtoram.si | 207 | 2 | 1 (handler 1, right OOB; 1 non-handler-caused, filtered) |
| stm32_uart_twoboards_comit.si | 285 | 2 | 1 (handler 1, right OOB; 1 non-handler-caused, filtered) |

All results confirmed by running the analyzer on the generated files.

---

## Optimization Comparison

The default analyzer run uses both handler optimizations:

| Analyzer flag | Selective handler application | Compiled handler fixpoint |
|---|---|---|
| default | on | on |
| `-selectoff` | off | on |
| `-compileoff` | on | off |
| `-optoff` | off | off |

`-selectoff` disables the yield-point filter, so handlers are applied after
every enabled expression instead of only at yield points. `-compileoff` disables
the compiled handler fixpoint and uses the iterative handler-summary path.
`-optoff` disables both optimizations.

The benchmark runner compares any selected mode against the default optimized
run:

```
python3 benchmark/run_benchmarks.py --md
python3 benchmark/run_benchmarks.py --selectoff --md
python3 benchmark/run_benchmarks.py --compileoff --md
python3 benchmark/run_benchmarks.py --optoff --md
python3 benchmark/run_benchmarks.py --compare-all --md
```

In benchmark tables, `Candidates` counts OOB candidates after the trace merge
step, while `Warnings` counts the interrupt-caused candidates retained by
provenance tracing. The merge step combines repeated observations of the same
access site, access kind, and base allocation across fixpoint iterations. In
comparison tables, `C <mode>` and `W <mode>` are the merged candidate and
warning counts for that optimization mode. `NoOpt` means `-optoff`, `SelOff`
means `-selectoff`, and `CompOff` means `-compileoff`.
`Spd <mode>` is `mode_time / opt_time`, so values above `1.00x` mean the
optimized default is faster.
