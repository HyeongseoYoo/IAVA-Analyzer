#pragma once

namespace bench11 {

class LinkUpHandler {
public:
  static LinkUpHandler &GetInstance();
  void OnInterruptHandler();

private:
  LinkUpHandler() = default;
};

} // namespace bench11
