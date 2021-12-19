// return 1 for macosx, 0 otherwise (assumed to be unix)
int system_type() {
  #if __APPLE__
  return 1;
  #else
  return 0;
  #endif
}
