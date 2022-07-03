#include <stddef.h>
#include <stdlib.h>
#include <string.h>

extern int *mkArray(size_t len) { return (int *)malloc(len * sizeof(int)); }
extern int addElement(int *array, size_t index, int element) {
  if (array + index) {
    array[index] = element;
    return 0;
  } else {
    return -1;
  }
}
extern int *addBetween(int *array, int element, size_t former_index,
                       size_t len) {
  int *result = (int *)malloc((len + 1) * sizeof(int));
  if (!result || !array) {
    return NULL;
  }
  memcpy(result, array, (former_index + 1) * sizeof(int));
  result[former_index + 1] = element;
  memcpy(result + former_index + 2, array + former_index + 1,
         sizeof(int) * (len - 1 - former_index));
  free(array);
  return result;
}