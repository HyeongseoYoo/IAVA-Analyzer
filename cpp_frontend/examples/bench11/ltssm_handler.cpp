#include "ltssm_handler.h"

#include "bench11_types.h"
#include "link_register_block.h"
#include "pcie_firmware.h"

namespace bench11 {

LtssmHandler &LtssmHandler::GetInstance() {
  static LtssmHandler instance;
  return instance;
}

void LtssmHandler::OnInterruptHandler() {
  PcieFirmware &fw = PcieFirmware::GetInstance();
  LinkRegisterBlock &regs = LinkRegisterBlock::GetInstance();
  ++fw.RecovCount;
  // [PATTERN-2 BUG] Writes INVALID_LINK (255) into the shared LinkRegisterBlock
  // at REG_LTSSMPTR. LinkController::GetLtssmPtr() reads the same register field;
  // if this fires before that read in PcieFirmware::ProcessTask, the value is
  // used to index StatBuf[16] — out-of-bounds.
  regs.Write(REG_LTSSMPTR, INVALID_LINK);
  regs.Write(REG_STATUS, LTSSM_RECOV);
  regs.Write(REG_FLAGS, LINK_RECOVERY);
  if (fw.EventHead < EVENT_LOG_SZ) {
    fw.EventLog[fw.EventHead] = MARK_RECOVERY;
  }
  ++fw.EventHead;
  ++fw.EventCount;
}

} // namespace bench11
