#include "nvme_register_block.h"

namespace bench9 {

NvmeRegisterBlock &NvmeRegisterBlock::GetInstance() {
  static NvmeRegisterBlock instance;
  return instance;
}

void NvmeRegisterBlock::Init() { regs_.fill(0); }

int NvmeRegisterBlock::Read(int index) const { return regs_[index]; }
void NvmeRegisterBlock::Write(int index, int value) { regs_[index] = value; }

} // namespace bench9
