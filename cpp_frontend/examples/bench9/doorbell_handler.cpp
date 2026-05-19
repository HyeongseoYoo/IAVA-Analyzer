#include "doorbell_handler.h"

#include "bench9_types.h"
#include "nvme_firmware.h"
#include "nvme_register_block.h"

namespace bench9 {

DoorbellHandler &DoorbellHandler::GetInstance() {
  static DoorbellHandler instance;
  return instance;
}

void DoorbellHandler::OnInterruptHandler() {
  NvmeFirmware &fw = NvmeFirmware::GetInstance();
  NvmeRegisterBlock &regs = NvmeRegisterBlock::GetInstance();
  ++fw.DoorbellCount;
  ++fw.SqTail;
  regs.Write(REG_SQTAIL, fw.SqTail);
  if (fw.EventHead < EVENT_LOG_SZ) {
    fw.EventLog[fw.EventHead] = MARK_DOORBELL;
  }
  ++fw.EventHead;
  while (0 < 0) {
  }
}

} // namespace bench9
