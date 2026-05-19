#include "error_controller.h"

#include "bench10_types.h"
#include "error_register_block.h"

namespace bench10 {

ErrorController &ErrorController::GetInstance() {
  static ErrorController instance;
  return instance;
}

int ErrorController::GetErrPtr() const {
  return ErrorRegisterBlock::GetInstance().Read(REG_ERRPTR);
}

void ErrorController::SetRegister(int index, int value) {
  ErrorRegisterBlock::GetInstance().Write(index, value);
}

} // namespace bench10
