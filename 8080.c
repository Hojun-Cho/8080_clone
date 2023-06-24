#include "8080.h"

void i8080_step(i8080 *const c);
void i8080_exec(i8080 *const c, uint8_t opcode);

static inline
uint16_t i8080_hl(i8080 *const c)
{
	return (c->h << 8) | c->l;
}

static inline
uint16_t i8080_bc(i8080 *const c)
{
	return (c->b << 8) | c->c;
}

static inline
uint16_t i8080_de(i8080 *const c)
{
	return (c->d << 8) | c->e;
}

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

/*
 * if i8080_add set cf flag:
 *	cf = false
 * else 
 *	cf = true
 * @param val : ((~val), !cf ) it's operation of  two's complement
 * @param cf  : 0 if SUB else 1 , case of else is SUB
*/
static inline
void i8080_sub(i8080 *const c, uint8_t *reg, uint8_t val, bool cf)
{
	i8080_add(c, reg, ~val, !cf);
	c->cf = !c->cf;
}

static inline
uint8_t i8080_inr(i8080 *const c, uint8_t val)
{
	uint8_t result = val + 1;
	c->hf = (result & 0xf) == 0;
	i8080_set_zsp(c, val);
	return result;
}

static inline
void i8080_inrm(i8080 *const c)
{
	uint16_t addr = i8080_hl(c);
	uint8_t val = i8080_inr(c, c->read_byte(c, addr));
	c->write_byte(c, addr, val);
}

static inline
uint8_t i8080_dec(i8080 *const c, uint8_t val)
{
	uint8_t result = val - 1;
	c->hf = !((result & 0xf) == 0xf);
	i8080_set_zsp(c, val);
	return result;
}

static inline
void i8080_decm(i8080 *const c)
{
	uint16_t addr = i8080_hl(c);
	uint8_t val = i8080_dec(c, c->read_byte(c, addr));
	c->write_byte(c, addr, val);
}

static inline
void i8080_inx_bc(i8080 *const c)
{
	uint16_t val = i8080_bc(c) + 1;
	c->b = val >> 8;
	c->c = val & 0xff;
}

static inline
void i8080_inx_de(i8080 *const c)
{
	uint16_t val = i8080_de(c) + 1;
	c->d = val >> 8;
	c->e = val & 0xff;
}

static inline
void i8080_inx_hl(i8080 *const c)
{
	uint16_t val = i8080_hl(c) + 1;
	c->h = val >> 8;
	c->l = val & 0xff;
}

static inline
void i8080_inx_sp(i8080 *const c)
{
	c->sp += 1;
}

static inline
void i8080_dcx_bc(i8080 *const c)
{
	uint16_t val = i8080_bc(c) - 1;
	c->b = val >> 8;
	c->c = val & 0xff;
}

static inline
void i8080_dcx_de(i8080 *const c)
{
	uint16_t val = i8080_de(c) - 1;
	c->d = val >> 8;
	c->e = val & 0xff;
}

/*The content of the register pair rp is added to the*/
/*content of the register pair Hand L. The result is*/
/*placed in the register pair Hand L. Note: Only the*/
/*CY flag is affected. It is set if there is a carry out of*/
/*the double precision add; otherwise it is reset.*/
static inline
void i8080_dad(i8080 *const c, uint16_t val)
{
	val += i8080_hl(c);
	 // how to set cf ??? i don't know not yet...
	c->cf = 0;	
	c->h = val >> 8;
	c->l = val & 0xff;
}

static inline
void i8080_dcx_hl(i8080 *const c)
{
	uint16_t val = i8080_hl(c) - 1;
	c->h = val >> 8;
	c->l = val & 0xff;
}

static inline
void i8080_dcx_sp(i8080 *const c)
{
	c->sp -= 1;
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
		// SUB R
		case 0x90: i8080_sub(c, &c->a, c->b, 0); break;
		case 0x91: i8080_sub(c, &c->a, c->c, 0); break;
		case 0x92: i8080_sub(c, &c->a, c->d, 0); break;
		case 0x93: i8080_sub(c, &c->a, c->e, 0); break;
		case 0x94: i8080_sub(c, &c->a, c->h, 0); break;
		case 0x95: i8080_sub(c, &c->a, c->l, 0); break;
		case 0x97: i8080_sub(c, &c->a, c->a, 0); break;
		// SBB R
		case 0x98: i8080_sub(c, &c->a, c->b, c->cf); break;
		case 0x99: i8080_sub(c, &c->a, c->c, c->cf); break;
		case 0x9a: i8080_sub(c, &c->a, c->d, c->cf); break;
		case 0x9b: i8080_sub(c, &c->a, c->e, c->cf); break;
		case 0x9c: i8080_sub(c, &c->a, c->h, c->cf); break;
		case 0x9d: i8080_sub(c, &c->a, c->l, c->cf); break;
		case 0x9f: i8080_sub(c, &c->a, c->a, c->cf); break;
		// SUB M
		case 0x96:
			i8080_sub(c, &c->a, c->read_byte(c, i8080_hl(c)), 0);
			break;	
		// SBB M
		case 0x9e:
			i8080_sub(c, &c->a, c->read_byte(c, i8080_hl(c)), c->cf);
			break;
		// SUI
		case 0xd6:
			i8080_sub(c, &c->a, c->read_byte(c, c->pc++), 0);
			break;
		// SBI
		case 0xde:
			i8080_sub(c, &c->a, c->read_byte(c, c->pc++), c->cf);
			break;
		// INR
		case 0x04: c->b = i8080_inr(c, c->b); break;
		case 0x14: c->d = i8080_inr(c, c->d); break;
		case 0x24: c->h = i8080_inr(c, c->h); break;
		case 0x0c: c->c = i8080_inr(c, c->c); break;
		case 0x1c: c->e = i8080_inr(c, c->e); break;
		case 0x2c: c->l = i8080_inr(c, c->l); break;
		case 0x3c: c->a = i8080_inr(c, c->a); break;
		// DCR
		case 0x05: c->b = i8080_dec(c, c->b); break;
		case 0x15: c->d = i8080_dec(c, c->d); break;
		case 0x25: c->h = i8080_dec(c, c->h); break;
		case 0x0d: c->c = i8080_dec(c, c->c); break;
		case 0x1d: c->e = i8080_dec(c, c->e); break;
		case 0x2d: c->l = i8080_dec(c, c->l); break;
		case 0x3d: c->a = i8080_dec(c, c->a); break;
		// INR M
		case 0x34: i8080_inrm(c); break;
		// DCR M
		case 0x35: i8080_decm(c); break;
		// INX
		case 0x03: i8080_inx_bc(c); break;
		case 0x13: i8080_inx_de(c); break;
		case 0x23: i8080_inx_hl(c); break;
		case 0x33: i8080_inx_sp(c); break;
		// DCX
		case 0x0b: i8080_dcx_bc(c); break;
		case 0x1b: i8080_dcx_de(c); break;
		case 0x2b: i8080_dcx_hl(c); break;
		case 0x3b: i8080_dcx_sp(c); break;
		// DAD
		case 0x09: i8080_dad(c, i8080_bc(c)); break;
		case 0x19: i8080_dad(c, i8080_de(c)); break;
		case 0x29: i8080_dad(c, i8080_hl(c)); break;
		case 0x39: i8080_dad(c, c->sp); break;
		// NOP
		case 0x00:
		case 0x10:
		case 0x20:
		case 0x30:
		case 0x08:
		case 0x18:
		case 0x28:
		case 0x38:
			break;
	}
}

void step(i8080 *const c)
{
	i8080_exec(c, c->read_byte(c, c->pc++));
}
