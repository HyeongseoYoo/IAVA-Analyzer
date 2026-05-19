#include "retry_handler.h"

#include "bench10_types.h"
#include "error_register_block.h"
#include "uecc_firmware.h"

namespace bench10 {

RetryHandler &RetryHandler::GetInstance() {
  static RetryHandler instance;
  return instance;
}

void RetryHandler::OnInterruptHandler() {
  UeccFirmware &fw = UeccFirmware::GetInstance();
  ErrorRegisterBlock &regs = ErrorRegisterBlock::GetInstance();
  ++fw.RetryCount;
  regs.Write(REG_RETRYCNT, fw.RetryCount);
  regs.Write(REG_STATUS, ERR_STATE_RETRY);
  if (fw.EventHead < EVENT_LOG_SZ) {
    fw.EventLog[fw.EventHead] = MARK_RETRY;
  }
  ++fw.EventHead;
}

} // namespace bench10
