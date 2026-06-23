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
| `ABORT_SLOT` | 255 | out-of-bounds sentinel written by handlers |
| `REG_PRPPTR` | 1 | field index written/read through aliases |

Handler assignments:

| Handler | Event | Effect | Bug source for |
|---|---|---|---|
| 0 | `NVME_ERR_EVT` | `SlotIdx := ABORT_SLOT` | micro1 |
| 1 | `TIMEOUT_EVT` | `*RegFile[REG_PRPPTR] := ABORT_SLOT` | micro2 |
| 2 | `CQ_FULL_EVT` | `DmaPtr := DmaConf; *DmaPtr[REG_PRPPTR] := ABORT_SLOT` | micro3 |

No handler writes `TxIdx`, `TxBuf`, or `DiagBuf`, so micro4's OOB is non-handler-caused.

---

## Micro benchmarks

### micro1_direct_buggy.si / micro1_direct_fixed.si — Pattern 1: direct scalar

**Bug site:** `*SqBuf[SlotIdx]` (line 70 of buggy file)

`SlotIdx` is checked in the `if SlotIdx < MAX_SLOTS` condition, but handler 0
can fire at the yield point between the check and the `then`-branch body,
overwriting `SlotIdx` with `ABORT_SLOT` (255) before the `SqBuf` write.  At
the access site the abstract value of `SlotIdx` is `[0, 255]`; `SqBuf` has 8
entries.

| | Expected |
|---|---|
| Buggy | 1 warning — `interrupt influence: handler 0`, right OOB `[8, 255]` |
| Fixed | 0 warnings |

**Fix strategy:** wrap the entire `if` in `disable/enable`:
`disable; if SlotIdx < MAX_SLOTS then *SqBuf[SlotIdx] := ERR_NONE else unit; enable`.
Inside the critical section no handler can fire, so the check on `SlotIdx`
is stable and the write is provably safe.

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

Both composites contain all three interrupt bug patterns and one non-handler OOB
in a single realistic scenario, testing detection and precision together.
Line counts: composite1 ≈ 961, composite2 ≈ 966.

---

### composite1_nvme_sqcq.si — NVMe SQ/CQ Command Processing Pipeline (~961 lines)

Models a full NVMe HIL pipeline: capability negotiation → namespace enumeration →
queue init → PRP list construction → 5-opcode command dispatch → CQ entry
staging → CQ drain → error/abort handling → LBA validation → ring reconciliation →
event-log aggregation → NS info page → feature-set update → PRP consistency check.

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

**Expected:** 4 bugs in total; Bug-1/2/3 show `interrupt influence: handler N` in the
`-prov` report, while the true-negative is filtered out of that report entirely.

---

### composite2_uecc_dma.si — UECC Error Detection + DMA PRP-List Pipeline (~966 lines)

Models a full UECC/DMA error-recovery pipeline: ECC engine init → channel/plane
preflight → page state init → DMA descriptor construction → read-request issue →
UECC correction (5-syndrome dispatch) → DMA PRP-list traversal → retry queue →
channel recovery → power-state management → syndrome re-scan → error aggregation
→ link snapshot → page reconciliation → channel utilization → plane finalization
→ ECC histogram → DMA teardown → error budget check → ECC engine re-arm.

`FATAL_SLOT = 200` deliberately differs from the micro suite's 255, demonstrating
that OOB ranges follow the handler's sentinel value.

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

**Expected:** 4 bugs in total; Bug-1/2/3 show `interrupt influence: handler N` in the
`-prov` report, while the true-negative is filtered out of that report entirely.

---

## Verification

Run with `-prov` flag:

```
./_build/default/bin/main.exe -prov benchmark/<file>.si
```

| File | Lines | Expected bugs | Interrupt warnings |
|---|---|---|---|
| micro1_direct_buggy.si | 73 | 1 | 1 (handler 0) |
| micro1_direct_fixed.si | 75 | 0 | 0 |
| micro2_alias1_buggy.si | 76 | 1 | 1 (handler 1) |
| micro2_alias1_fixed.si | 82 | 0 | 0 |
| micro3_aliasmulti_buggy.si | 81 | 1 | 1 (handler 2) |
| micro3_aliasmulti_fixed.si | 80 | 0 | 0 |
| micro4_nonhandler_oob.si | 75 | 1 | 0 (true-negative) |
| composite1_nvme_sqcq.si | 961 | 4 | 3 (one per handler) |
| composite2_uecc_dma.si | 966 | 4 | 3 (one per handler) |

All results confirmed by running the analyzer on the generated files.
