# Codex Bug Report

The 25 reports are mostly duplicates of 5 underlying bugs: shared index variables are modified by interrupts after main has checked them, and some register fields are shared through aliases.

**Bugs #25, #24, #23, #22, #21, #20, #19, #17, #9: `CmdSlot` can become 255**

Main reason: `CmdSlot` is used as an index into 16-entry arrays, but handler 0 can asynchronously overwrite it with `INVALID_CID = 255`.

Bad accesses:

- #25 line 230: `*SqBuf[CmdSlot]`
- #24 line 231: `*CqBuf[CmdSlot]`
- #23 line 232: `*ErrLog[CmdSlot]`
- #22 line 245: `*SqBuf[CmdSlot]`
- #21 line 259: `*SqBuf[CmdSlot]`
- #20 line 260: `*CqBuf[CmdSlot]`
- #19 line 264: `*SqBuf[CmdSlot]`
- #17 line 270: `*SqBuf[CmdSlot]`
- #9 line 327: `*ErrLog[CmdSlot]`

Trace:

- line 57: `INVALID_CID := 255`
- handler 0 `NVME_ERR_EVT`, line 153: `CmdSlot := INVALID_CID`
- main checks loops such as `while CmdSlot < MAX_CMDS`, but an interrupt can fire after the check and before the array access
- arrays are only 16 entries:
  - line 115: `SqBuf := malloc(16, CMD_FREE)`
  - line 116: `CqBuf := malloc(16, 0)`
  - line 118: `ErrLog := malloc(16, ERR_NONE)`

Concrete fix: do not let the ISR write directly to `CmdSlot`. Use a separate flag such as `AbortRequested := 1`, or disable interrupts around the check-and-access sequence. Also copy `CmdSlot` to a local validated index and use only that local for the array access.

---

**Bugs #15: `NvmeRegs[REG_PRPPTR]` can become 255 through handler 1**

Main reason: main bounds-checks `*NvmeCtrl[REG_PRPPTR]`, but handler 1 can change the same register field before the indexed write.

Bad access:

- #15 line 282: `*PrpList[*NvmeCtrl[REG_PRPPTR]]`

Trace:

- line 117: `PrpList := malloc(16, 0)`
- line 121: `NvmeRegs := malloc(16, 0)`
- line 128: `*NvmeRegs[REG_PRPPTR] := 0`
- line 214: `NvmeCtrl := NvmeRegs`, so `NvmeCtrl` and `NvmeRegs` refer to the same register block
- handler 1 `TIMEOUT_EVT`, line 169: `*NvmeRegs[REG_PRPPTR] := INVALID_CID`
- main line 280 checks `*NvmeCtrl[REG_PRPPTR] < PRP_LIST_SZ`
- main line 282 reads `*NvmeCtrl[REG_PRPPTR]` again as the index, which may now be 255

Concrete fix: read the PRP pointer once into a local while interrupts are disabled, validate that local, then use the same local for indexing:

```si
disable;
prp := *NvmeCtrl[REG_PRPPTR];
enable;
if prp < PRP_LIST_SZ then *PrpList[prp] := CmdSlot else ...
```

---

**Bug #14: `DmaConfig[REG_PRPPTR]` can become 255 through handler 2 aliasing**

Main reason: main accesses `DmaConfig` indirectly through `EngineRef`, while handler 2 writes to the same object through `HandlerDmaPtr`.

Bad access:

- #14 line 295: `*PrpList2[*EngineRef[REG_PRPPTR]]`

Trace:

- line 122: `PrpList2 := malloc(16, 0)`
- line 123: `DmaConfig := malloc(16, 0)`
- line 124: `DmaDescTable := malloc(4, 0)`
- line 140: `*DmaConfig[REG_PRPPTR] := 0`
- line 139: `*DmaDescTable[0] := DmaConfig`
- line 224: `SysRef := DmaDescTable`
- line 225: `EngineRef := *SysRef[0]`, so `EngineRef` aliases `DmaConfig`
- handler 2 `CQ_FULL_EVT`, line 189: `HandlerDmaPtr := DmaConfig`
- handler 2 line 190: `*HandlerDmaPtr[REG_PRPPTR] := INVALID_CID`
- main checks `*EngineRef[REG_PRPPTR] < PRP_LIST_SZ`, then rereads it at line 295; the second read may be 255

Concrete fix: same pattern as above: snapshot `*EngineRef[REG_PRPPTR]` into a local under interrupt protection, validate the local, and index with the local. Alternatively, make handler 2 update a separate status field instead of corrupting the PRP pointer.

---

**Bugs #13, #12, #10: `CqHead` can grow past `CqBuf[16]`**

Main reason: `CqHead` is the main loop index for `CqBuf`, but handler 2 also increments `CqHead`.

Bad accesses:

- #13 line 312: `*CqBuf[CqHead]` read
- #12 line 314: `*CqBuf[CqHead]` write
- #10 line 319: `*CqBuf[CqHead]` write

Trace:

- line 116: `CqBuf := malloc(16, 0)`
- line 309: `CqHead := 0`
- main loop checks `while CqHead < MAX_CMDS`
- handler 2 `CQ_FULL_EVT`, line 186: `CqHead := CqHead + 1`
- main line 321 also does `CqHead := CqHead + 1`
- because both main and the ISR advance the same index, `CqHead` can exceed 15 before the next `CqBuf[CqHead]` access

Concrete fix: separate ISR-owned CQ state from main’s drain-loop index. For example, use `CqIsrHead` in handler 2 and a local `DrainIdx` in main. If they must share `CqHead`, protect the loop condition and all `CqBuf[CqHead]` accesses with interrupts disabled.

---

**Bugs #3, #2, #1, #18, #16, #11, #8, #7, #6, #5, #4: `EventHead` can grow past `EventLog[32]`**

Main reason: `EventHead` is a shared append index into `EventLog[32]`. Main and all four handlers increment it without wrapping or synchronization. Some handler writes check `EventHead < EVENT_LOG_SZ`, but then still increment unbounded; main’s final snapshot writes do not check at all.

Bad accesses:

- #3 line 159: handler 0 `*EventLog[EventHead]`
- #2 line 174: handler 1 `*EventLog[EventHead]`
- #1 line 192: handler 2 `*EventLog[EventHead]`
- #18 line 265: main `*EventLog[EventHead]`
- #16 line 271: main `*EventLog[EventHead]`
- #11 line 315: main `*EventLog[EventHead]`
- #8 line 350: main `*EventLog[EventHead]`
- #7 line 352: main `*EventLog[EventHead]`
- #6 line 354: main `*EventLog[EventHead]`
- #5 line 356: main `*EventLog[EventHead]`
- #4 line 358: main `*EventLog[EventHead]`

Trace:

- line 119: `EventLog := malloc(32, 0)`
- line 109: `EventHead := 0`
- line 239: `EventHead := 2`
- main increments at lines 266, 272, 316, 351, 353, 355, 357, 359
- handlers also increment:
  - handler 0 line 161: `EventHead := EventHead + 1`
  - handler 1 line 176: `EventHead := EventHead + 1`
  - handler 2 line 194: `EventHead := EventHead + 1`
  - handler 3 line 205: `EventHead := EventHead + 1`
- once `EventHead >= 32`, every `*EventLog[EventHead]` write is out of bounds

Concrete fix: make event logging a bounded ring buffer:

```si
if EventHead < EVENT_LOG_SZ
then *EventLog[EventHead] := value
else unit;
EventHead := (EventHead + 1) % EVENT_LOG_SZ
```

Also make the write-plus-increment atomic with respect to interrupts, or centralize event logging in one routine that disables interrupts while updating `EventHead`.

