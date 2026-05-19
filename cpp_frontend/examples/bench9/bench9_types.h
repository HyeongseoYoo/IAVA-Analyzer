#pragma once

namespace bench9 {

enum NvmeConst : int {
  MAX_CMDS = 16,
  PRP_LIST_SZ = 16,
  CQ_BUF_SZ = 16,
  SQ_BUF_SZ = 16,
  ERR_LOG_SZ = 16,
  EVENT_LOG_SZ = 32,
  DIAG_BUF_SZ = 8,
  NVME_REG_SZ = 16,

  INVALID_CID = 255,
  ERR_NONE = 0,
  ERR_INVAL_CMD = 1,
  ERR_TIMEOUT = 2,
  ERR_ABORT = 3,
  ERR_MEDIA = 4,
  ERR_INTERNAL = 5,
  ERR_DATA = 6,
  ERR_POWER = 7,

  CMD_FREE = 0,
  CMD_PENDING = 1,
  CMD_ACTIVE = 2,
  CMD_COMPLETE = 3,
  CMD_ABORT = 4,
  CMD_ERROR = 5,

  REG_STATUS = 0,
  REG_PRPPTR = 1,
  REG_SQTAIL = 2,
  REG_CQHEAD = 3,
  REG_ERRCNT = 4,
  REG_CMDCNT = 5,
  REG_ABORTCNT = 6,
  REG_FLAGS = 7,
  REG_TIMEOUT = 8,
  REG_LASTCID = 9,

  MARK_SUBMIT = 10,
  MARK_COMPLETE = 20,
  MARK_ERROR = 30,
  MARK_ABORT = 40,
  MARK_TIMEOUT = 50,
  MARK_DOORBELL = 60,
  MARK_FLUSH = 70,
  MARK_RESET = 80
};

struct Task {
  int opcode;
};

} // namespace bench9
