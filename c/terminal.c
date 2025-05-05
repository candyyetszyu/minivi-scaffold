#include <sys/ioctl.h>
#include <unistd.h>

int term_row() {
  struct winsize w;
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) == -1)
    return -1;
  return w.ws_row;
}

int term_col() {
  struct winsize w;
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) == -1)
    return -1;
  return w.ws_col;
}