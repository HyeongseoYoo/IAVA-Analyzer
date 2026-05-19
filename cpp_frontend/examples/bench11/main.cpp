#include "bench11_pcie.h"

int main() {
  using namespace bench11;

  PcieFirmware::GetInstance().Init();
  TaskQueue::GetInstance().PushTask({0});

  while (TaskQueue::GetInstance().HasTask()) {
    Task task = TaskQueue::GetInstance().PopTask();
    PcieFirmware::GetInstance().ProcessTask(task);
  }

  return 0;
}
