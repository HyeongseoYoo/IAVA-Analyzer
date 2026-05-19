#pragma once

namespace bench9 {

class NvmeErrHandler {
public:
  static NvmeErrHandler &GetInstance();
  void OnInterruptHandler();

private:
  NvmeErrHandler() = default;
};

} // namespace bench9
