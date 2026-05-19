#include "nvme_controller.h"

#include "bench9_types.h"
#include "nvme_register_block.h"

namespace bench9 {

NvmeController &NvmeController::GetInstance() {
  static NvmeController instance;
  return instance;
}

int NvmeController::GetPrpPtr() const {
  return NvmeRegisterBlock::GetInstance().Read(REG_PRPPTR);
}

void NvmeController::SetRegister(int index, int value) {
  NvmeRegisterBlock::GetInstance().Write(index, value);
}

} // namespace bench9
