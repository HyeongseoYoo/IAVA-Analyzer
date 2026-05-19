#include "link_controller.h"

#include "bench11_types.h"
#include "link_register_block.h"

namespace bench11 {

LinkController &LinkController::GetInstance() {
  static LinkController instance;
  return instance;
}

int LinkController::GetLtssmPtr() const {
  return LinkRegisterBlock::GetInstance().Read(REG_LTSSMPTR);
}

void LinkController::SetRegister(int index, int value) {
  LinkRegisterBlock::GetInstance().Write(index, value);
}

} // namespace bench11
