# Codex Bug Report

The 22 reports reduce to five root causes. Several bug IDs are repeated symptoms of the same corrupted index.

**Bugs #19-#22, #14-#18, #6: `CmdSlot` corrupted by handler 0**

Main reason: `CmdSlot` is used as an array index for 16-entry arrays, but interrupt handler 0 can overwrite it with `INVALID_CID = 255`.

Trace:
- Line 55: `INVALID_CID := 255`
- Handler 0, line 150: `CmdSlot := INVALID_CID`
- Main later uses `CmdSlot` as an index:
  - Lines 225, 240, 255, 261, 267: `*SqBuf[CmdSlot]`
  - Lines 226, 256: `*CqBuf[CmdSlot]`
  - Lines 227, 324: `*ErrLog[CmdSlot]`
- These arrays are only 16 entries:
  - Line 112: `SqBuf := malloc(16, CMD_FREE)`
  - Line 113: `CqBuf := malloc(16, 0)`
  - Line 115: `ErrLog := malloc(16, ERR_NONE)`

Handler involved: `handler 0` / `NVME_ERR_EVT`.

Concrete fix: do not let the ISR write directly to the loop/index variable. Use a separate flag or status variable, for example `AbortRequested := 1`, and let main handle it at a safe point. Also keep `CmdSlot` local/protected while indexing, or disable interrupts around the check-and-access sequence.

---

**Bugs #12: `NvmeRegs[REG_PRPPTR]` corrupted by handler 1**

Main reason: main checks `*NvmeCtrl[REG_PRPPTR]`, but handler 1 can change the same register field to `255` before main uses it as an index into `PrpList[16]`.

Trace:
- Line 118: `NvmeRegs := malloc(16, 0)`
- Line 125: `*NvmeRegs[REG_PRPPTR] := 0`
- Line 209: `NvmeCtrl := NvmeRegs`, so `NvmeCtrl` aliases `NvmeRegs`
- Handler 1, line 167: `*NvmeRegs[REG_PRPPTR] := INVALID_CID`
- Line 279: main writes `*PrpList[*NvmeCtrl[REG_PRPPTR]]`
- Line 114: `PrpList := malloc(16, 0)`, so valid indexes are `0..15`; `255` is OOB.

Handler involved: `handler 1` / `TIMEOUT_EVT`.

Concrete fix: read the PRP pointer once into a local variable while interrupts are disabled, validate that local value, then use the same local value for the array access. Alternatively, make handler 1 write a separate abort/status field instead of corrupting `REG_PRPPTR`.

---

**Bug #11: `DmaConfig[REG_PRPPTR]` corrupted through hidden alias by handler 2**

Main reason: main reaches `DmaConfig` through `DmaDescTable`, while handler 2 reaches the same object directly. Handler 2 stores `255` into the PRP pointer field, and main later uses that field as an index into `PrpList2[16]`.

Trace:
- Line 120: `DmaConfig := malloc(16, 0)`
- Line 121: `DmaDescTable := malloc(4, 0)`
- Line 136: `*DmaDescTable[0] := DmaConfig`
- Line 137: `*DmaConfig[REG_PRPPTR] := 0`
- Line 219: `SysRef := DmaDescTable`
- Line 220: `EngineRef := *SysRef[0]`, so `EngineRef` aliases `DmaConfig`
- Handler 2:
  - Line 187: `HandlerDmaPtr := DmaConfig`
  - Line 188: `*HandlerDmaPtr[REG_PRPPTR] := INVALID_CID`
- Line 292: main writes `*PrpList2[*EngineRef[REG_PRPPTR]]`
- Line 119: `PrpList2 := malloc(16, 0)`, so index `255` is OOB.

Handler involved: `handler 2` / `CQ_FULL_EVT`.

Concrete fix: do not store `INVALID_CID` into a field that is later used as an array index. Use a separate validity flag or sentinel field. Also snapshot `*EngineRef[REG_PRPPTR]` once, validate the snapshot, and use that same snapshot for the write.

---

**Bugs #10, #9, #7: `CqHead` grows past `CqBuf[16]`**

Main reason: `CqHead` is shared between main and handler 2. Both increment it, so the loop condition `CqHead < MAX_CMDS` does not guarantee the later `CqBuf[CqHead]` access is still in range.

Trace:
- Line 113: `CqBuf := malloc(16, 0)`
- Line 306: main sets `CqHead := 0`
- Handler 2, line 184: `CqHead := CqHead + 1`
- Main also increments at line 318: `CqHead := CqHead + 1`
- Main accesses:
  - Line 309: `*CqBuf[CqHead]` read
  - Lines 311 and 316: `*CqBuf[CqHead]` writes
- Once `CqHead >= 16`, these are OOB.

Handler involved: `handler 2` / `CQ_FULL_EVT`.

Concrete fix: protect `CqHead` updates with interrupt masking or make main use a local snapshot for indexing. If it is a ring buffer, update with wraparound: `CqHead := (CqHead + 1) % CQ_BUF_SZ`.

---

**Bugs #15, #13, #8, #5-#1: `EventHead` grows past `EventLog[32]`**

Main reason: `EventHead` is a shared log index. Main and all four handlers increment it, but many main writes do not guard or wrap the index. Eventually it can exceed the 32-entry `EventLog`.

Trace:
- Line 116: `EventLog := malloc(32, 0)`
- Line 234: `EventHead := 2`
- Handlers increment it:
  - Handler 0, line 158: `EventHead := EventHead + 1`
  - Handler 1, line 174: `EventHead := EventHead + 1`
  - Handler 2, line 192: `EventHead := EventHead + 1`
  - Handler 3, line 203: `EventHead := EventHead + 1`
- Main also increments it at lines 263, 269, 313, 348, 350, 352, 354
- OOB writes occur at:
  - Lines 262, 268, 312, 347, 349, 351, 353, 355: `*EventLog[EventHead]`

Handlers involved: `handler 0`, `handler 1`, `handler 2`, `handler 3`.

Concrete fix: make event logging bounded everywhere. Either wrap the index with `EventHead := (EventHead + 1) % EVENT_LOG_SZ`, or guard every write and do not increment past the end. The handlers and main should use the same logging helper/policy so the bound is enforced consistently.

