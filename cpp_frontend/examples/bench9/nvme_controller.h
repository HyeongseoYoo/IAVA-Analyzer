#pragma once

namespace bench9 {

class NvmeController {
public:
  static NvmeController &GetInstance();
  int GetPrpPtr() const;
  void SetRegister(int index, int value);

private:
  NvmeController() = default;
};

} // namespace bench9
