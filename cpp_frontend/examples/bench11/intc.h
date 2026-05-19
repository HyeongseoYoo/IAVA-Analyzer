#pragma once

namespace bench11 {

class INTC {
public:
  static INTC &GetInstance();
  static void DisableInterrupt();
  static void EnableInterrupt();
  static bool IsEnabled();

private:
  INTC() = default;
  static bool enabled_;
};

} // namespace bench11
