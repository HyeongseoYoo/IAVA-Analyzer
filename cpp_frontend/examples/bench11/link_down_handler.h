#pragma once

namespace bench11 {

class LinkDownHandler {
public:
  static LinkDownHandler &GetInstance();
  void OnInterruptHandler();

private:
  LinkDownHandler() = default;
};

} // namespace bench11
