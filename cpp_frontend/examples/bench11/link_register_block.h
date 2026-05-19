#pragma once

#include <array>

#include "bench11_types.h"

namespace bench11 {

class LinkRegisterBlock {
public:
  static LinkRegisterBlock &GetInstance();
  void Init();
  int Read(int index) const;
  void Write(int index, int value);

private:
  LinkRegisterBlock() = default;
  std::array<int, LINK_REG_SZ> regs_{};
};

} // namespace bench11
