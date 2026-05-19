#pragma once

#include <array>

#include "bench9_types.h"

namespace bench9 {

class NvmeFirmware {
public:
  static NvmeFirmware &GetInstance();
  void Init();
  void ProcessTask(const Task &task);

  int CmdSlot = 0;
  int CqHead = 0;
  int SqTail = 0;
  int CmdPending = 0;
  int CmdDone = 0;
  int ErrCode = ERR_NONE;
  int ErrCount = 0;
  int AbortCount = 0;
  int TimeoutCount = 0;
  int DoorbellCount = 0;
  int EventHead = 0;
  int EventCount = 0;
  int LastPrpPtr = 0;
  int DiagIdx = 0;

  std::array<int, SQ_BUF_SZ> SqBuf{};
  std::array<int, CQ_BUF_SZ> CqBuf{};
  std::array<int, PRP_LIST_SZ> PrpList{};
  std::array<int, ERR_LOG_SZ> ErrLog{};
  std::array<int, EVENT_LOG_SZ> EventLog{};
  std::array<int, DIAG_BUF_SZ> DiagBuf{};

private:
  NvmeFirmware() = default;
};

} // namespace bench9
