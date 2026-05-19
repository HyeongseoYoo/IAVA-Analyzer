#pragma once

#include <array>

#include "bench9_types.h"

namespace bench9 {

class NvmeRegisterBlock {
public:
  static NvmeRegisterBlock &GetInstance();
  void Init();
  int Read(int index) const;
  void Write(int index, int value);

private:
  NvmeRegisterBlock() = default;
  std::array<int, NVME_REG_SZ> regs_{};
};

} // namespace bench9
