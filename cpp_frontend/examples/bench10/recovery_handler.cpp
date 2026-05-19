#include "recovery_handler.h"

#include "bench10_types.h"
#include "error_register_block.h"
#include "uecc_firmware.h"

namespace bench10 {

RecoveryHandler &RecoveryHandler::GetInstance() {
  static RecoveryHandler instance;
  return instance;
}

void RecoveryHandler::OnInterruptHandler() {
  UeccFirmware &fw = UeccFirmware::GetInstance();
  ErrorRegisterBlock &regs = ErrorRegisterBlock::GetInstance();
  ++fw.RecovCount;
  fw.ErrState = ERR_STATE_OK;
  regs.Write(REG_STATUS, ERR_STATE_RECOV);
  regs.Write(REG_FLAGS, ERR_NONE);
  if (fw.EventHead < EVENT_LOG_SZ) {
    fw.EventLog[fw.EventHead] = MARK_RECOV;
  }
  ++fw.EventHead;
  while (0 < 0) {
  }
}

} // namespace bench10
