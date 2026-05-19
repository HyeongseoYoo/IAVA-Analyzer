#pragma once

namespace bench10 {

enum UeccConst : int {
  MAX_ERR_SLOTS = 16,
  ERR_LOG_SZ = 16,
  RETRY_BUF_SZ = 16,
  STAT_LOG_SZ = 8,
  EVENT_LOG_SZ = 32,
  DIAG_BUF_SZ = 8,
  ERR_REG_SZ = 16,

  FATAL_SLOT = 200,
  ERR_NONE = 0,
  ERR_UECC = 1,
  ERR_MEDIA = 2,
  ERR_TIMEOUT = 3,
  ERR_OVERTEMP = 4,
  ERR_WEAR = 5,
  ERR_BAD_BLOCK = 6,
  ERR_ECC_LIMIT = 7,

  ERR_STATE_OK = 0,
  ERR_STATE_WARN = 1,
  ERR_STATE_CRIT = 2,
  ERR_STATE_FATAL = 3,
  ERR_STATE_RETRY = 4,
  ERR_STATE_RECOV = 5,

  REG_STATUS = 0,
  REG_ERRPTR = 1,
  REG_ERRTYPE = 2,
  REG_RETRYCNT = 3,
  REG_ERRCNT = 4,
  REG_BADBLK = 5,
  REG_TEMP = 6,
  REG_WEAR = 7,
  REG_FLAGS = 8,
  REG_LASTSLOT = 9,

  MARK_UECC = 10,
  MARK_MEDIA = 20,
  MARK_RETRY = 30,
  MARK_RECOV = 40,
  MARK_TIMEOUT = 50,
  MARK_WARN = 60,
  MARK_FATAL = 70,
  MARK_RESET = 80
};

struct Task {
  int opcode;
};

} // namespace bench10
