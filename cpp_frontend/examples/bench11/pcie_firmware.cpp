#include "pcie_firmware.h"

#include "link_controller.h"
#include "link_register_block.h"

namespace bench11 {

PcieFirmware &PcieFirmware::GetInstance() {
  static PcieFirmware instance;
  return instance;
}

void PcieFirmware::Init() {
  LinkSlot = 0;
  LinkState = LTSSM_DETECT;
  LinkSpeed = 0;
  LinkWidth = 0;
  ErrCount = 0;
  LinkUpCount = 0;
  LinkDownCount = 0;
  RecovCount = 0;
  PmState = 0;
  EventHead = 0;
  EventCount = 0;
  LastLtssmPtr = 0;
  DiagIdx = 0;

  LinkTable.fill(LTSSM_DETECT);
  LinkBuf.fill(0);
  StatBuf.fill(0);
  PmLog.fill(0);
  EventLog.fill(0);
  DiagBuf.fill(0);
  LinkRegisterBlock::GetInstance().Init();
}

void PcieFirmware::ProcessTask(const Task &task) {
  (void)task;
  LinkController &LinkCtrl = LinkController::GetInstance();
  LinkRegisterBlock &regs = LinkRegisterBlock::GetInstance();

  // Phase 1: flush link table, link buffer, and stat buffer
  LinkSlot = 0;
  while (LinkSlot < MAX_LINKS) {
    LinkTable[LinkSlot] = LTSSM_DETECT;
    LinkBuf[LinkSlot] = 0;
    StatBuf[LinkSlot] = 0;
    ++LinkSlot;
  }
  LinkSlot = 0;

  EventLog[0] = MARK_RESET;
  EventLog[1] = LTSSM_DETECT;
  EventHead = 2;
  DiagBuf[0] = LinkSlot;
  DiagBuf[1] = LinkState;

  // Phase 2: pre-load link table with polling state
  while (LinkSlot < MAX_LINKS) {
    LinkTable[LinkSlot] = LTSSM_POLLING;
    ++LinkSlot;
  }
  LinkSlot = 0;

  // Phase 3: main link dispatch loop
  // [TARGET-1, Pattern 1] LinkSlot is checked in the while condition;
  // handler 0 can fire after the check and set LinkSlot to INVALID_LINK (255),
  // so the subsequent LinkTable and LinkBuf accesses become out-of-bounds.
  while (LinkSlot < MAX_LINKS) {
    DiagBuf[2] = LinkSlot;
    DiagBuf[3] = LinkState;

    // [BUG-1] Handler 0 may have set LinkSlot to INVALID_LINK before this access
    LinkTable[LinkSlot] = LinkState;
    LinkBuf[LinkSlot] = ErrCount;

    if (LinkState == LINK_DOWN) {
      EventLog[EventHead] = MARK_LINKDOWN;
      ++EventHead;
      LinkTable[LinkSlot] = LINK_DOWN;
    } else {
      if (LinkState == LINK_RECOVERY) {
        EventLog[EventHead] = MARK_RECOVERY;
        ++EventHead;
        LinkTable[LinkSlot] = LINK_RECOVERY;
      } else {
        EventLog[EventHead] = MARK_TRAINING;
        ++EventHead;
        ++LinkUpCount;
        regs.Write(REG_LINKCNT, LinkUpCount);
      }
    }

    // [TARGET-2, Pattern 2] Read LTSSM pointer through LinkCtrl (aliases LinkRegs).
    // Handler 1 may have set LinkRegs[REG_LTSSMPTR] to INVALID_LINK before this read.

    // [BUG-2] LastLtssmPtr could be INVALID_LINK; StatBuf has only 16 entries
    LastLtssmPtr = LinkCtrl.GetLtssmPtr();
    StatBuf[LastLtssmPtr] = LinkState;

    DiagBuf[4] = LastLtssmPtr;
    DiagBuf[5] = ErrCount;
    DiagBuf[6] = LinkUpCount;
    DiagBuf[7] = LinkDownCount;

    ++LinkSlot;
    regs.Write(REG_LASTLINK, LinkSlot);
  }

  // Phase 4: link buffer drain loop
  LinkSlot = 0;
  while (LinkSlot < MAX_LINKS) {
    DiagBuf[0] = LinkSlot;
    if (LinkBuf[LinkSlot] == LINK_DOWN) {
      LinkBuf[LinkSlot] = LINK_RECOVERY;
      EventLog[EventHead] = MARK_RECOVERY;
      ++EventHead;
    } else {
      LinkBuf[LinkSlot] = LINK_OK;
    }
    ++LinkSlot;
  }

  // Phase 5: stat buffer scan and statistics
  LinkSlot = 0;
  while (LinkSlot < MAX_LINKS) {
    if (StatBuf[LinkSlot] == LINK_DOWN) {
      PmLog[0] = LinkSlot;
    } else {
      PmLog[1] = StatBuf[LinkSlot];
      ++LinkUpCount;
    }
    ++LinkSlot;
  }

  regs.Write(REG_ERRCNT, ErrCount);
  regs.Write(REG_LINKCNT, LinkUpCount);
  regs.Write(REG_STATUS, LTSSM_L0);

  PmLog[2] = ErrCount;
  PmLog[3] = LinkUpCount;
  PmLog[4] = LinkDownCount;
  PmLog[5] = RecovCount;
  PmLog[6] = PmState;
  PmLog[7] = EventCount;

  DiagBuf[0] = ErrCount;
  DiagBuf[1] = LinkUpCount;
  DiagBuf[2] = LinkDownCount;
  DiagBuf[3] = RecovCount;
  DiagBuf[4] = PmState;
  DiagBuf[5] = EventCount;
  DiagBuf[6] = LastLtssmPtr;
  DiagBuf[7] = LinkState;

  // Phase 6: final PCIe state snapshot
  EventLog[EventHead] = MARK_RESET;     ++EventHead;
  EventLog[EventHead] = LinkState;      ++EventHead;
  EventLog[EventHead] = ErrCount;       ++EventHead;
  EventLog[EventHead] = LinkUpCount;    ++EventHead;
  EventLog[EventHead] = LinkDownCount;  ++EventHead;
  EventLog[EventHead] = RecovCount;     ++EventHead;
  regs.Write(REG_STATUS, LTSSM_L0);
  regs.Write(REG_LINKCNT, LinkUpCount);
  regs.Write(REG_ERRCNT, ErrCount);
  regs.Write(REG_LASTLINK, LinkSlot);
}

} // namespace bench11
