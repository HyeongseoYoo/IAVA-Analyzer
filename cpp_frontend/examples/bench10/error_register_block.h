#pragma once

#include <array>

#include "bench10_types.h"

namespace bench10 {

class ErrorRegisterBlock {
public:
  static ErrorRegisterBlock &GetInstance();
  void Init();
  int Read(int index) const;
  void Write(int index, int value);

private:
  ErrorRegisterBlock() = default;
  std::array<int, ERR_REG_SZ> regs_{};
};

} // namespace bench10
