#pragma once

#include <array>

#include "bench11_types.h"

namespace bench11 {

class PcieFirmware {
public:
  static PcieFirmware &GetInstance();
  void Init();
  void ProcessTask(const Task &task);

  int LinkSlot = 0;
  int LinkState = LTSSM_DETECT;
  int LinkSpeed = 0;
  int LinkWidth = 0;
  int ErrCount = 0;
  int LinkUpCount = 0;
  int LinkDownCount = 0;
  int RecovCount = 0;
  int PmState = 0;
  int EventHead = 0;
  int EventCount = 0;
  int LastLtssmPtr = 0;
  int DiagIdx = 0;

  std::array<int, MAX_LINKS> LinkTable{};
  std::array<int, LINK_BUF_SZ> LinkBuf{};
  std::array<int, STAT_BUF_SZ> StatBuf{};
  std::array<int, PM_LOG_SZ> PmLog{};
  std::array<int, EVENT_LOG_SZ> EventLog{};
  std::array<int, DIAG_BUF_SZ> DiagBuf{};

private:
  PcieFirmware() = default;
};

} // namespace bench11
