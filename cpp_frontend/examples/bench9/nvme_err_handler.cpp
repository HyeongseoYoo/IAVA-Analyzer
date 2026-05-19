#include "nvme_err_handler.h"

#include "bench9_types.h"
#include "nvme_firmware.h"
#include "nvme_register_block.h"

namespace bench9 {

NvmeErrHandler &NvmeErrHandler::GetInstance() {
  static NvmeErrHandler instance;
  return instance;
}

void NvmeErrHandler::OnInterruptHandler() {
  NvmeFirmware &fw = NvmeFirmware::GetInstance();
  NvmeRegisterBlock &regs = NvmeRegisterBlock::GetInstance();
  fw.ErrCode = ERR_INVAL_CMD;
  ++fw.ErrCount;
  ++fw.AbortCount;
  // [PATTERN-1 BUG] Overwrites CmdSlot with INVALID_CID (255).
  // If this fires between the while-condition check (CmdSlot < MAX_CMDS) and
  // the SqBuf/CqBuf accesses in NvmeFirmware::ProcessTask, those accesses use
  // index 255 into 16-element arrays — out-of-bounds.
  fw.CmdSlot = INVALID_CID;
  regs.Write(REG_STATUS, ERR_INVAL_CMD);
  regs.Write(REG_ERRCNT, fw.ErrCount);
  regs.Write(REG_ABORTCNT, fw.AbortCount);
  regs.Write(REG_FLAGS, ERR_INVAL_CMD);
  if (fw.EventHead < EVENT_LOG_SZ) {
    fw.EventLog[fw.EventHead] = MARK_ERROR;
  }
  ++fw.EventHead;
  ++fw.EventCount;
}

} // namespace bench9
