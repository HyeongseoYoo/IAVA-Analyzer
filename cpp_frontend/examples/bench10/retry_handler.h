#pragma once

namespace bench10 {

class RetryHandler {
public:
  static RetryHandler &GetInstance();
  void OnInterruptHandler();

private:
  RetryHandler() = default;
};

} // namespace bench10
