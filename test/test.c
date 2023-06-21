#include <assert.h>
#include <stdint.h>
#include <stdio.h>

int is_parity(uint8_t val)
{
	val ^= (val >> 4);
	val ^= (val >> 2);
	val ^= (val >> 1);
	return val & 1; 
}

void assert_parity()
{
	for (uint8_t i = 0x0; i < 0xff; i++)
		assert(is_parity(i) == __builtin_parity(i));
	assert(is_parity(0xff) == __builtin_parity(0xff));
}

/*20 : 0001 0100*/
  /*-> 1110 1011*/
  /*+1 1110 1100*/

/*-20 : 1110 1100*/
   /*-> 0001 0011*/
   /*+1 0001 0100*/

	/*0000 1010*/
      /*+ 0001 0100*/
/*res:    0001 1110*/

// 0001 0011
void sub_uint8()
{
	uint8_t x = 10;
	uint8_t y = -20;
	uint8_t neg_y = ~y + 1;
	uint8_t res = x + neg_y;
	
	assert ((int8_t)res == 30);	
}

int main()
{
	assert_parity();
	sub_uint8();
}
