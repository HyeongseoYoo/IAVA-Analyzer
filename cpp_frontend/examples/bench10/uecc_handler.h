#pragma once

namespace bench10 {

class UeccHandler {
public:
  static UeccHandler &GetInstance();
  void OnInterruptHandler();

private:
  UeccHandler() = default;
};

} // namespace bench10
