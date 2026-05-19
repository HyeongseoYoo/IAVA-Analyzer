#include "uecc_handler.h"

#include "bench10_types.h"
#include "error_register_block.h"
#include "uecc_firmware.h"

namespace bench10 {

UeccHandler &UeccHandler::GetInstance() {
  static UeccHandler instance;
  return instance;
}

void UeccHandler::OnInterruptHandler() {
  UeccFirmware &fw = UeccFirmware::GetInstance();
  ErrorRegisterBlock &regs = ErrorRegisterBlock::GetInstance();
  fw.ErrState = ERR_STATE_FATAL;
  fw.ErrCode = ERR_UECC;
  ++fw.ErrCount;
  ++fw.UeccCount;
  // [PATTERN-1 BUG] Overwrites ErrSlot with FATAL_SLOT (200).
  // If this fires between the while-condition check (ErrSlot < MAX_ERR_SLOTS) and
  // the ErrTable/RetryBuf accesses in UeccFirmware::ProcessTask, those accesses
  // use index 200 into 16-element arrays — out-of-bounds.
  fw.ErrSlot = FATAL_SLOT;
  regs.Write(REG_STATUS, ERR_STATE_FATAL);
  regs.Write(REG_ERRTYPE, ERR_UECC);
  regs.Write(REG_ERRCNT, fw.ErrCount);
  regs.Write(REG_FLAGS, ERR_UECC);
  if (fw.EventHead < EVENT_LOG_SZ) {
    fw.EventLog[fw.EventHead] = MARK_UECC;
  }
  ++fw.EventHead;
  ++fw.EventCount;
}

} // namespace bench10
