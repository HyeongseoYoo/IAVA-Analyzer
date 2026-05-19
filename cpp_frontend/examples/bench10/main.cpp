#include "bench10_uecc.h"

int main() {
  using namespace bench10;

  UeccFirmware::GetInstance().Init();
  TaskQueue::GetInstance().PushTask({0});

  while (TaskQueue::GetInstance().HasTask()) {
    Task task = TaskQueue::GetInstance().PopTask();
    UeccFirmware::GetInstance().ProcessTask(task);
  }

  return 0;
}
