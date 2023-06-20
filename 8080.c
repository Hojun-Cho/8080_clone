#include "8080.h"

void i8080_step(i8080 *const c);
void i8080_exec(i8080 *const c, uint8_t opcode);

static inline
bool is_parity(uint8_t val)
{
	val ^= (val >> 4);
	val ^= (val >> 2);
	val ^= (val >> 1);
	return val & 1; 
}

static inline
void i8080_set_zsp(i8080 *const c, uint8_t val)
{
	if (val == 0)
		c->zf = true;
	if (val >> 7)
		c->sf = true;
	if (is_parity(val))
		c->sf = true;
}

static inline
bool i8080_carry(uint8_t bit_num, uint8_t a, uint8_t b, bool cf)
{
	uint8_t res = a + b + cf;
	uint8_t carry = res ^ a ^ b;
	return carry & ( 1 << bit_num);
}

static inline
void i8080_add(i8080 *const c, uint8_t *reg, uint8_t val, bool cf)
{
	uint8_t result = *reg + val + cf;
	c->hf = i8080_carry(4, *reg, val, cf);
	c->cf = i8080_carry(8, *reg, val, cf);
	*reg = result;
	i8080_set_zsp(c, result);
}

static inline
uint8_t i8080_hl(i8080 *const c)
{
	return (c->h << 8) | c->l;
}

void i8080_exec(i8080 *const c, uint8_t opcode)
{
	c->cyc += OPCODES_CYCLES[opcode];

	switch (opcode) 
	{
		// ADD
		case 0x80: i8080_add(c, &c->a, c->b, 0); break;
		case 0x81: i8080_add(c, &c->a, c->c, 0); break;
		case 0x82: i8080_add(c, &c->a, c->d, 0); break;
		case 0x83: i8080_add(c, &c->a, c->e, 0); break;
		case 0x84: i8080_add(c, &c->a, c->h, 0); break;
		case 0x85: i8080_add(c, &c->a, c->l, 0); break;
		case 0x87: i8080_add(c, &c->a, c->a, 0); break;
		// ADC 
		case 0x88: i8080_add(c, &c->a, c->b, c->cf); break;
		case 0x89: i8080_add(c, &c->a, c->c, c->cf); break;
		case 0x8a: i8080_add(c, &c->a, c->d, c->cf); break;
		case 0x8b: i8080_add(c, &c->a, c->e, c->cf); break;
		case 0x8c: i8080_add(c, &c->a, c->h, c->cf); break;
		case 0x8d: i8080_add(c, &c->a, c->l, c->cf); break;
		case 0x8f: i8080_add(c, &c->a, c->a, c->cf); break;
		// ADC M
		case 0x8e:
			i8080_add(c, &c->a, c->read_byte(c, i8080_hl(c)), c->cf);
			break;		
		// ADD M
		case 0x86: i8080_add(c, &c->a, c->read_byte(c, i8080_hl(c)), 0);
			break;
		// ADI
		case 0xc6:
			i8080_add(c, &c->a, c->read_byte(c, c->pc++), 0);
			break;
		// ACI
		case 0xce:
			i8080_add(c, &c->a, c->read_byte(c, c->pc++), c->cf);
			break;
		case 0x00:
		case 0x10:
		case 0x20:
		case 0x30:
		case 0x08:
		case 0x18:
		case 0x28:
		case 0x38:
			break; //NOP
	}
}

void step(i8080 *const c)
{
	i8080_exec(c, c->read_byte(c, c->pc++));
}
