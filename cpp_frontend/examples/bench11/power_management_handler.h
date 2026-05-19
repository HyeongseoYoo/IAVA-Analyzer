#pragma once

namespace bench11 {

class PowerManagementHandler {
public:
  static PowerManagementHandler &GetInstance();
  void OnInterruptHandler();

private:
  PowerManagementHandler() = default;
};

} // namespace bench11
