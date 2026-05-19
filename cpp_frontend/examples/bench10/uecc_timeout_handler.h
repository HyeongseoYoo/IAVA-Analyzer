#pragma once

namespace bench10 {

class UeccTimeoutHandler {
public:
  static UeccTimeoutHandler &GetInstance();
  void OnInterruptHandler();

private:
  UeccTimeoutHandler() = default;
};

} // namespace bench10
