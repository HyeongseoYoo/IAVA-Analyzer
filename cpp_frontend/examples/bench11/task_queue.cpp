#include "task_queue.h"

namespace bench11 {

TaskQueue &TaskQueue::GetInstance() {
  static TaskQueue instance;
  return instance;
}

void TaskQueue::PushTask(const Task &task) {
  task_ = task;
  has_task_ = true;
}

bool TaskQueue::HasTask() const { return has_task_; }

Task TaskQueue::PopTask() {
  has_task_ = false;
  return task_;
}

} // namespace bench11
