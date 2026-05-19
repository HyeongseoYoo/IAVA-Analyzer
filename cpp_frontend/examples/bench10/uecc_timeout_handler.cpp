#include "uecc_timeout_handler.h"

#include "bench10_types.h"
#include "error_register_block.h"
#include "uecc_firmware.h"

namespace bench10 {

UeccTimeoutHandler &UeccTimeoutHandler::GetInstance() {
  static UeccTimeoutHandler instance;
  return instance;
}

void UeccTimeoutHandler::OnInterruptHandler() {
  UeccFirmware &fw = UeccFirmware::GetInstance();
  ErrorRegisterBlock &regs = ErrorRegisterBlock::GetInstance();
  ++fw.TimeoutCount;
  ++fw.ErrCount;
  // [PATTERN-2 BUG] Writes FATAL_SLOT (200) into the shared ErrorRegisterBlock
  // at REG_ERRPTR. ErrorController::GetErrPtr() reads the same register field;
  // if this fires before that read in UeccFirmware::ProcessTask, the value is
  // used to index ErrLog[16] — out-of-bounds.
  regs.Write(REG_ERRPTR, FATAL_SLOT);
  regs.Write(REG_ERRCNT, fw.ErrCount);
  regs.Write(REG_FLAGS, ERR_TIMEOUT);
  if (fw.EventHead < EVENT_LOG_SZ) {
    fw.EventLog[fw.EventHead] = MARK_TIMEOUT;
  }
  ++fw.EventHead;
  ++fw.EventCount;
}

} // namespace bench10
