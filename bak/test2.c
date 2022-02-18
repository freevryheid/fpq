#include <stdio.h>
#include <string.h>
#include <unistd.h>

int
main() {
  char *user;
  int length;
  user = getlogin();
  if (user) {
    length = strlen(user);
    printf("user: %s\n", user);
    printf("len: %d\n", length);
  } else {
    printf("No user found!\n");
  }
  return 0;
}

