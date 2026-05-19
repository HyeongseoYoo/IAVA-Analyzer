#pragma once

namespace bench10 {

class RecoveryHandler {
public:
  static RecoveryHandler &GetInstance();
  void OnInterruptHandler();

private:
  RecoveryHandler() = default;
};

} // namespace bench10
