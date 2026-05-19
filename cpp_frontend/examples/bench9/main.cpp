#include "bench9_nvme.h"

int main() {
  using namespace bench9;

  NvmeFirmware::GetInstance().Init();
  TaskQueue::GetInstance().PushTask({0});

  while (TaskQueue::GetInstance().HasTask()) {
    Task task = TaskQueue::GetInstance().PopTask();
    NvmeFirmware::GetInstance().ProcessTask(task);
  }

  return 0;
}
