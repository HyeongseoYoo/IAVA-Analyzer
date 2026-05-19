#pragma once

namespace bench9 {

class TimeoutHandler {
public:
  static TimeoutHandler &GetInstance();
  void OnInterruptHandler();

private:
  TimeoutHandler() = default;
};

} // namespace bench9
