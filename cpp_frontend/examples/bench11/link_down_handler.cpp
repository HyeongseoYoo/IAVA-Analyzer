#include "link_down_handler.h"

#include "bench11_types.h"
#include "link_register_block.h"
#include "pcie_firmware.h"

namespace bench11 {

LinkDownHandler &LinkDownHandler::GetInstance() {
  static LinkDownHandler instance;
  return instance;
}

void LinkDownHandler::OnInterruptHandler() {
  PcieFirmware &fw = PcieFirmware::GetInstance();
  LinkRegisterBlock &regs = LinkRegisterBlock::GetInstance();
  fw.LinkState = LINK_DOWN;
  ++fw.ErrCount;
  ++fw.LinkDownCount;
  // [PATTERN-1 BUG] Overwrites LinkSlot with INVALID_LINK (255).
  // If this fires between the while-condition check (LinkSlot < MAX_LINKS) and
  // the LinkTable/LinkBuf accesses in PcieFirmware::ProcessTask, those accesses
  // use index 255 into 16-element arrays — out-of-bounds.
  fw.LinkSlot = INVALID_LINK;
  regs.Write(REG_STATUS, LINK_DOWN);
  regs.Write(REG_ERRCNT, fw.ErrCount);
  regs.Write(REG_FLAGS, LINK_DOWN);
  if (fw.EventHead < EVENT_LOG_SZ) {
    fw.EventLog[fw.EventHead] = MARK_LINKDOWN;
  }
  ++fw.EventHead;
  ++fw.EventCount;
}

} // namespace bench11
