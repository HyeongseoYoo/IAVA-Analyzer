#pragma once

#include <array>

#include "bench10_types.h"

namespace bench10 {

class UeccFirmware {
public:
  static UeccFirmware &GetInstance();
  void Init();
  void ProcessTask(const Task &task);

  int ErrSlot = 0;
  int ErrState = ERR_STATE_OK;
  int ErrCode = ERR_NONE;
  int ErrCount = 0;
  int RetryCount = 0;
  int RecovCount = 0;
  int UeccCount = 0;
  int MediaErrCount = 0;
  int TimeoutCount = 0;
  int BadBlkCount = 0;
  int EventHead = 0;
  int EventCount = 0;
  int LastErrPtr = 0;
  int DiagIdx = 0;

  std::array<int, MAX_ERR_SLOTS> ErrTable{};
  std::array<int, RETRY_BUF_SZ> RetryBuf{};
  std::array<int, ERR_LOG_SZ> ErrLog{};
  std::array<int, STAT_LOG_SZ> StatLog{};
  std::array<int, EVENT_LOG_SZ> EventLog{};
  std::array<int, DIAG_BUF_SZ> DiagBuf{};

private:
  UeccFirmware() = default;
};

} // namespace bench10
