#pragma once

namespace bench10 {

class ErrorController {
public:
  static ErrorController &GetInstance();
  int GetErrPtr() const;
  void SetRegister(int index, int value);

private:
  ErrorController() = default;
};

} // namespace bench10
