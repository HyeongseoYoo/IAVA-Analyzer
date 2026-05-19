#pragma once

namespace bench10 {

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

} // namespace bench10
