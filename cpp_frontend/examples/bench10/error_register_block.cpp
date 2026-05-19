#include "error_register_block.h"

namespace bench10 {

ErrorRegisterBlock &ErrorRegisterBlock::GetInstance() {
  static ErrorRegisterBlock instance;
  return instance;
}

void ErrorRegisterBlock::Init() { regs_.fill(0); }

int ErrorRegisterBlock::Read(int index) const { return regs_[index]; }
void ErrorRegisterBlock::Write(int index, int value) { regs_[index] = value; }

} // namespace bench10
