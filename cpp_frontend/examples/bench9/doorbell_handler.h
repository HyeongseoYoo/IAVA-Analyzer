#pragma once

namespace bench9 {

class DoorbellHandler {
public:
  static DoorbellHandler &GetInstance();
  void OnInterruptHandler();

private:
  DoorbellHandler() = default;
};

} // namespace bench9
