#pragma once

namespace bench11 {

class LtssmHandler {
public:
  static LtssmHandler &GetInstance();
  void OnInterruptHandler();

private:
  LtssmHandler() = default;
};

} // namespace bench11
