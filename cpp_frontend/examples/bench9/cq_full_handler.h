#pragma once

namespace bench9 {

class CqFullHandler {
public:
  static CqFullHandler &GetInstance();
  void OnInterruptHandler();

private:
  CqFullHandler() = default;
};

} // namespace bench9
