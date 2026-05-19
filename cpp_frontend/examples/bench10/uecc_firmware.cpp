#include "uecc_firmware.h"

#include "error_controller.h"
#include "error_register_block.h"

namespace bench10 {

UeccFirmware &UeccFirmware::GetInstance() {
  static UeccFirmware instance;
  return instance;
}

void UeccFirmware::Init() {
  ErrSlot = 0;
  ErrState = ERR_STATE_OK;
  ErrCode = ERR_NONE;
  ErrCount = 0;
  RetryCount = 0;
  RecovCount = 0;
  UeccCount = 0;
  MediaErrCount = 0;
  TimeoutCount = 0;
  BadBlkCount = 0;
  EventHead = 0;
  EventCount = 0;
  LastErrPtr = 0;
  DiagIdx = 0;

  ErrTable.fill(ERR_STATE_OK);
  RetryBuf.fill(0);
  ErrLog.fill(ERR_NONE);
  StatLog.fill(0);
  EventLog.fill(0);
  DiagBuf.fill(0);
  ErrorRegisterBlock::GetInstance().Init();
}

void UeccFirmware::ProcessTask(const Task &task) {
  (void)task;
  ErrorController &ErrCtrl = ErrorController::GetInstance();
  ErrorRegisterBlock &regs = ErrorRegisterBlock::GetInstance();

  // Phase 1: flush error table, retry buffer, and error log
  ErrSlot = 0;
  while (ErrSlot < MAX_ERR_SLOTS) {
    ErrTable[ErrSlot] = ERR_STATE_OK;
    RetryBuf[ErrSlot] = 0;
    ErrLog[ErrSlot] = ERR_NONE;
    ++ErrSlot;
  }
  ErrSlot = 0;

  EventLog[0] = MARK_RESET;
  EventLog[1] = ERR_STATE_OK;
  EventHead = 2;
  DiagBuf[0] = ErrSlot;
  DiagBuf[1] = ErrState;

  // Phase 2: pre-load error table with warning state
  while (ErrSlot < MAX_ERR_SLOTS) {
    ErrTable[ErrSlot] = ERR_STATE_WARN;
    ++ErrSlot;
  }
  ErrSlot = 0;

  // Phase 3: main error dispatch loop
  // [TARGET-1, Pattern 1] ErrSlot is checked in the while condition;
  // handler 0 can fire after the check and set ErrSlot to FATAL_SLOT (200),
  // so the subsequent ErrTable and RetryBuf accesses become out-of-bounds.
  while (ErrSlot < MAX_ERR_SLOTS) {
    DiagBuf[2] = ErrSlot;
    DiagBuf[3] = ErrState;

    // [BUG-1] Handler 0 may have set ErrSlot to FATAL_SLOT before this access
    ErrTable[ErrSlot] = ErrState;
    RetryBuf[ErrSlot] = ErrCount;

    if (ErrState == ERR_STATE_FATAL) {
      EventLog[EventHead] = MARK_FATAL;
      ++EventHead;
      ErrTable[ErrSlot] = ERR_STATE_FATAL;
    } else {
      if (ErrState == ERR_STATE_CRIT) {
        EventLog[EventHead] = MARK_WARN;
        ++EventHead;
        ErrTable[ErrSlot] = ERR_STATE_CRIT;
      } else {
        EventLog[EventHead] = MARK_MEDIA;
        ++EventHead;
        ++MediaErrCount;
        regs.Write(REG_STATUS, ERR_STATE_WARN);
      }
    }

    // [TARGET-2, Pattern 2] Read error pointer through ErrCtrl (aliases ErrRegs).
    // Handler 1 may have set ErrRegs[REG_ERRPTR] to FATAL_SLOT before this read.

    // [BUG-2] LastErrPtr could be FATAL_SLOT; ErrLog has only 16 entries
    LastErrPtr = ErrCtrl.GetErrPtr();
    ErrLog[LastErrPtr] = ErrCode;

    DiagBuf[4] = LastErrPtr;
    DiagBuf[5] = ErrCount;
    DiagBuf[6] = RetryCount;
    DiagBuf[7] = UeccCount;

    ++ErrSlot;
    regs.Write(REG_LASTSLOT, ErrSlot);
  }

  // Phase 4: retry buffer drain loop
  ErrSlot = 0;
  while (ErrSlot < MAX_ERR_SLOTS) {
    DiagBuf[0] = ErrSlot;
    if (RetryBuf[ErrSlot] == ERR_STATE_FATAL) {
      RetryBuf[ErrSlot] = ERR_STATE_RECOV;
      EventLog[EventHead] = MARK_RECOV;
      ++EventHead;
    } else {
      RetryBuf[ErrSlot] = ERR_NONE;
    }
    ++ErrSlot;
  }

  // Phase 5: error log scan and statistics
  ErrSlot = 0;
  while (ErrSlot < ERR_LOG_SZ) {
    if (ErrLog[ErrSlot] == ERR_NONE) {
      StatLog[0] = ErrSlot;
    } else {
      StatLog[1] = ErrLog[ErrSlot];
      ++ErrCount;
    }
    ++ErrSlot;
  }

  regs.Write(REG_ERRCNT, ErrCount);
  regs.Write(REG_RETRYCNT, RetryCount);
  regs.Write(REG_STATUS, ERR_STATE_OK);

  StatLog[2] = ErrCount;
  StatLog[3] = RetryCount;
  StatLog[4] = UeccCount;
  StatLog[5] = MediaErrCount;
  StatLog[6] = RecovCount;
  StatLog[7] = EventCount;

  DiagBuf[0] = ErrCount;
  DiagBuf[1] = RetryCount;
  DiagBuf[2] = UeccCount;
  DiagBuf[3] = MediaErrCount;
  DiagBuf[4] = RecovCount;
  DiagBuf[5] = TimeoutCount;
  DiagBuf[6] = EventCount;
  DiagBuf[7] = LastErrPtr;

  // Phase 6: final UECC state snapshot
  EventLog[EventHead] = MARK_RESET;  ++EventHead;
  EventLog[EventHead] = ErrState;    ++EventHead;
  EventLog[EventHead] = ErrCount;    ++EventHead;
  EventLog[EventHead] = RetryCount;  ++EventHead;
  EventLog[EventHead] = UeccCount;   ++EventHead;
  EventLog[EventHead] = RecovCount;  ++EventHead;
  regs.Write(REG_STATUS, ERR_STATE_OK);
  regs.Write(REG_ERRCNT, ErrCount);
  regs.Write(REG_RETRYCNT, RetryCount);
  regs.Write(REG_LASTSLOT, ErrSlot);
}

} // namespace bench10
