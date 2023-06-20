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


int main()
{
	assert_parity();
}
