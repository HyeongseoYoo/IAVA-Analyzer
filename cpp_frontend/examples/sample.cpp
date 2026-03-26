#include <cstdint>

// ===== Global constants / enums =====
enum UserMode : uint32_t { INIT = 0, SVC = 1, IRQ = 2, FIQ = 3, INVALID = 100 };

// ===== Global variables =====
static uint32_t CurrentUser;
static uint32_t Status[4];

// ===== Interrupt control stubs =====
void DisableInterrupts() {
  // 실제 환경에서는 CPU interrupt mask 조작
}

void EnableInterrupts() {
  // 실제 환경에서는 CPU interrupt mask 해제
}

// ===== Initialization =====
void InitSystem() {
  CurrentUser = INIT;

  for (int i = 0; i < 4; i++) {
    Status[i] = 0;
  }
}

// ===== Interrupt handler =====
void Handler0() { CurrentUser = INVALID; }

// ===== Main logic =====
void MainLogic() {
  if (CurrentUser == INVALID) {
    CurrentUser = INIT;
  } else {
    Status[CurrentUser] = 1;
  }
}

int main() {
  InitSystem();

  // MainLogic -> Target for analysis
  MainLogic();
  return 0;
}