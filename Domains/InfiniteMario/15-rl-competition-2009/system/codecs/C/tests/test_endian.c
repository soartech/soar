#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


#include <stdio.h>

#define LITTLE_ENDIAN 0
#define BIG_ENDIAN    1

int machineEndianness()
{
  int i = 1;
  char *p = (char *) &i;
  if (p[0] == 1) // Lowest address contains the least significant byte
    return LITTLE_ENDIAN;
  else
    return BIG_ENDIAN;
}




int main(int argc, char *argv[]) {
	if(machineEndianness()==LITTLE_ENDIAN)
		printf("This machine is LITTLE ENDIAN\n");
	if(machineEndianness()==BIG_ENDIAN)
		printf("This machine is BIGENDIAN\n");
		
		printf("Sizes: char %lu int %lu double %lu\n",sizeof(char),sizeof(int),sizeof(double));
}
