#pragma once

#include "bench10_types.h"

namespace bench10 {

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

} // namespace bench10
