#pragma once

#include "bench11_types.h"

namespace bench11 {

class TaskQueue {
public:
  static TaskQueue &GetInstance();
  void PushTask(const Task &task);
  bool HasTask() const;
  Task PopTask();

private:
  TaskQueue() = default;
  Task task_{0};
  bool has_task_ = false;
};

} // namespace bench11
