#include "nvme_firmware.h"

#include "nvme_controller.h"
#include "nvme_register_block.h"

namespace bench9 {

NvmeFirmware &NvmeFirmware::GetInstance() {
  static NvmeFirmware instance;
  return instance;
}

void NvmeFirmware::Init() {
  CmdSlot = 0;
  CqHead = 0;
  SqTail = 0;
  CmdPending = 0;
  CmdDone = 0;
  ErrCode = ERR_NONE;
  ErrCount = 0;
  AbortCount = 0;
  TimeoutCount = 0;
  DoorbellCount = 0;
  EventHead = 0;
  EventCount = 0;
  LastPrpPtr = 0;
  DiagIdx = 0;

  SqBuf.fill(CMD_FREE);
  CqBuf.fill(0);
  PrpList.fill(0);
  ErrLog.fill(ERR_NONE);
  EventLog.fill(0);
  DiagBuf.fill(0);
  NvmeRegisterBlock::GetInstance().Init();
}

void NvmeFirmware::ProcessTask(const Task &task) {
  (void)task;
  NvmeController &NvmeCtrl = NvmeController::GetInstance();
  NvmeRegisterBlock &regs = NvmeRegisterBlock::GetInstance();

  // Phase 1: flush SQ, CQ, and error log
  CmdSlot = 0;
  while (CmdSlot < MAX_CMDS) {
    SqBuf[CmdSlot] = CMD_FREE;
    CqBuf[CmdSlot] = 0;
    ErrLog[CmdSlot] = ERR_NONE;
    ++CmdSlot;
  }
  CmdSlot = 0;

  EventLog[0] = MARK_RESET;
  EventLog[1] = ERR_NONE;
  EventHead = 2;
  DiagBuf[0] = CmdSlot;
  DiagBuf[1] = ErrCode;

  // Phase 2: pre-load SQ with pending commands
  while (CmdSlot < MAX_CMDS) {
    SqBuf[CmdSlot] = CMD_PENDING;
    ++CmdSlot;
  }
  CmdSlot = 0;

  // Phase 3: main command dispatch loop
  // [TARGET-1, Pattern 1] CmdSlot is checked in the while condition;
  // handler 0 can fire after the check and set CmdSlot := 255, so the
  // subsequent SqBuf and CqBuf accesses become out-of-bounds.
  while (CmdSlot < MAX_CMDS) {
    DiagBuf[0] = CmdSlot;
    DiagBuf[1] = ErrCode;

    // [BUG-1] Handler 0 may have set CmdSlot to INVALID_CID before this access
    SqBuf[CmdSlot] = CMD_ACTIVE;
    CqBuf[CmdSlot] = ErrCode;

    if (ErrCode == ERR_NONE) {
      SqBuf[CmdSlot] = CMD_COMPLETE;
      EventLog[EventHead] = MARK_COMPLETE;
      ++EventHead;
      ++CmdDone;
    } else {
      SqBuf[CmdSlot] = CMD_ERROR;
      EventLog[EventHead] = MARK_ERROR;
      ++EventHead;
      ++ErrCount;
    }

    // [TARGET-2, Pattern 2] Read PRP pointer through NvmeCtrl (aliases NvmeRegs).
    // Handler 1 may have set NvmeRegs[REG_PRPPTR] to INVALID_CID before this read.

    // [BUG-2] LastPrpPtr could be INVALID_CID; PrpList has only 16 entries
    if (NvmeCtrl.GetPrpPtr() < PRP_LIST_SZ) {
      PrpList[NvmeCtrl.GetPrpPtr()] = CmdSlot;
    } else {
      PrpList[0] = CmdSlot;
    }

    DiagBuf[2] = LastPrpPtr;
    DiagBuf[3] = ErrCount;
    DiagBuf[4] = CmdDone;
    DiagBuf[5] = AbortCount;

    ++CmdSlot;
    regs.Write(REG_LASTCID, CmdSlot);
  }

  // Phase 4: CQ drain loop
  CqHead = 0;
  while (CqHead < MAX_CMDS) {
    DiagBuf[6] = CqHead;
    if (CqBuf[CqHead] == CMD_PENDING) {
      CqBuf[CqHead] = CMD_COMPLETE;
      EventLog[EventHead] = MARK_COMPLETE;
      ++EventHead;
    } else {
      CqBuf[CqHead] = 0;
    }
    ++CqHead;
  }

  // Phase 5: error log scan and statistics
  CmdSlot = 0;
  while (CmdSlot < ERR_LOG_SZ) {
    if (ErrLog[CmdSlot] == ERR_NONE) {
      DiagBuf[0] = CmdSlot;
    } else {
      DiagBuf[1] = ErrLog[CmdSlot];
      ++ErrCount;
    }
    ++CmdSlot;
  }

  regs.Write(REG_ERRCNT, ErrCount);
  regs.Write(REG_CMDCNT, CmdDone);
  regs.Write(REG_STATUS, ERR_NONE);

  DiagBuf[0] = ErrCount;
  DiagBuf[1] = CmdDone;
  DiagBuf[2] = AbortCount;
  DiagBuf[3] = TimeoutCount;
  DiagBuf[4] = DoorbellCount;
  DiagBuf[5] = EventCount;
  DiagBuf[6] = LastPrpPtr;
  DiagBuf[7] = ErrCode;

  // Phase 6: final NVMe state snapshot
  EventLog[EventHead] = MARK_FLUSH; ++EventHead;
  EventLog[EventHead] = ErrCount;   ++EventHead;
  EventLog[EventHead] = CmdDone;    ++EventHead;
  EventLog[EventHead] = AbortCount; ++EventHead;
  EventLog[EventHead] = TimeoutCount; ++EventHead;
  regs.Write(REG_STATUS, ERR_NONE);
  regs.Write(REG_CMDCNT, CmdDone);
  regs.Write(REG_ERRCNT, ErrCount);
  regs.Write(REG_LASTCID, CmdSlot);
}

} // namespace bench9
