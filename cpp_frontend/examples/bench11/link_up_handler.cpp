#include "link_up_handler.h"

#include "bench11_types.h"
#include "link_register_block.h"
#include "pcie_firmware.h"

namespace bench11 {

LinkUpHandler &LinkUpHandler::GetInstance() {
  static LinkUpHandler instance;
  return instance;
}

void LinkUpHandler::OnInterruptHandler() {
  PcieFirmware &fw = PcieFirmware::GetInstance();
  LinkRegisterBlock &regs = LinkRegisterBlock::GetInstance();
  ++fw.LinkUpCount;
  fw.LinkState = LTSSM_L0;
  regs.Write(REG_STATUS, LTSSM_L0);
  regs.Write(REG_LINKCNT, fw.LinkUpCount);
  regs.Write(REG_FLAGS, LINK_OK);
  if (fw.EventHead < EVENT_LOG_SZ) {
    fw.EventLog[fw.EventHead] = MARK_LINKUP;
  }
  ++fw.EventHead;
}

} // namespace bench11
