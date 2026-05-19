#include "intc.h"

namespace bench11 {

bool INTC::enabled_ = true;

INTC &INTC::GetInstance() {
  static INTC instance;
  return instance;
}

void INTC::DisableInterrupt() { enabled_ = false; }
void INTC::EnableInterrupt() { enabled_ = true; }
bool INTC::IsEnabled() { return enabled_; }

} // namespace bench11
