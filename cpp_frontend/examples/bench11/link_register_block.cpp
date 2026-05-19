#include "link_register_block.h"

namespace bench11 {

LinkRegisterBlock &LinkRegisterBlock::GetInstance() {
  static LinkRegisterBlock instance;
  return instance;
}

void LinkRegisterBlock::Init() { regs_.fill(0); }

int LinkRegisterBlock::Read(int index) const { return regs_[index]; }
void LinkRegisterBlock::Write(int index, int value) { regs_[index] = value; }

} // namespace bench11
