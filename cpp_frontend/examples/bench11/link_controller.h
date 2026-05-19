#pragma once

namespace bench11 {

class LinkController {
public:
  static LinkController &GetInstance();
  int GetLtssmPtr() const;
  void SetRegister(int index, int value);

private:
  LinkController() = default;
};

} // namespace bench11
