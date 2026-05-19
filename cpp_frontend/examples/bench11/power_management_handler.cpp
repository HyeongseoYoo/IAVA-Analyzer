#include "power_management_handler.h"

#include "bench11_types.h"
#include "link_register_block.h"
#include "pcie_firmware.h"

namespace bench11 {

PowerManagementHandler &PowerManagementHandler::GetInstance() {
  static PowerManagementHandler instance;
  return instance;
}

void PowerManagementHandler::OnInterruptHandler() {
  PcieFirmware &fw = PcieFirmware::GetInstance();
  LinkRegisterBlock &regs = LinkRegisterBlock::GetInstance();
  ++fw.PmState;
  regs.Write(REG_PMSTATE, fw.PmState);
  if (fw.EventHead < EVENT_LOG_SZ) {
    fw.EventLog[fw.EventHead] = MARK_PMENTER;
  }
  ++fw.EventHead;
  while (0 < 0) {
  }
}

} // namespace bench11
