#include "cq_full_handler.h"

#include "bench9_types.h"
#include "nvme_firmware.h"
#include "nvme_register_block.h"

namespace bench9 {

CqFullHandler &CqFullHandler::GetInstance() {
  static CqFullHandler instance;
  return instance;
}

void CqFullHandler::OnInterruptHandler() {
  NvmeFirmware &fw = NvmeFirmware::GetInstance();
  NvmeRegisterBlock &regs = NvmeRegisterBlock::GetInstance();
  ++fw.CmdDone;
  ++fw.CqHead;
  regs.Write(REG_CQHEAD, fw.CqHead);
  regs.Write(REG_CMDCNT, fw.CmdDone);
  if (fw.EventHead < EVENT_LOG_SZ) {
    fw.EventLog[fw.EventHead] = MARK_COMPLETE;
  }
  ++fw.EventHead;
}

} // namespace bench9
