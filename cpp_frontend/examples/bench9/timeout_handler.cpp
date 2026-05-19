#include "timeout_handler.h"

#include "bench9_types.h"
#include "nvme_firmware.h"
#include "nvme_register_block.h"

namespace bench9 {

TimeoutHandler &TimeoutHandler::GetInstance() {
  static TimeoutHandler instance;
  return instance;
}

void TimeoutHandler::OnInterruptHandler() {
  NvmeFirmware &fw = NvmeFirmware::GetInstance();
  NvmeRegisterBlock &regs = NvmeRegisterBlock::GetInstance();
  ++fw.TimeoutCount;
  ++fw.ErrCount;
  // [PATTERN-2 BUG] Writes INVALID_CID (255) into the shared NvmeRegisterBlock
  // at REG_PRPPTR. NvmeController::GetPrpPtr() reads the same register field;
  // if this fires before that read in NvmeFirmware::ProcessTask, the value is
  // used to index PrpList[16] — out-of-bounds.
  regs.Write(REG_PRPPTR, INVALID_CID);
  regs.Write(REG_ERRCNT, fw.ErrCount);
  regs.Write(REG_TIMEOUT, fw.TimeoutCount);
  regs.Write(REG_FLAGS, ERR_TIMEOUT);
  if (fw.EventHead < EVENT_LOG_SZ) {
    fw.EventLog[fw.EventHead] = MARK_TIMEOUT;
  }
  ++fw.EventHead;
  ++fw.EventCount;
}

} // namespace bench9
