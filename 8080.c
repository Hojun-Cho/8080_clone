#include "8080.h"

void i8080_step(i8080 *const c);
void i8080_exec(i8080 *const c, uint8_t opcode);

static inline
uint16_t i8080_read_word(i8080 *c, uint16_t addr)
{
	uint16_t val;

	val = c->read_byte(c, addr + 1);
	val <<= 8;
	val |= c->read_byte(c, addr);
	return val;
}

static inline
uint16_t i8080_next_word(i8080 *c)
{
	uint16_t val = i8080_read_word(c, c->pc);
	c->pc += 2;
	return val;
}

static inline
void i8080_write_word(i8080 *c, uint16_t addr, uint16_t val)
{
	c->write_byte(c, addr, val & 0xff);
	c->write_byte(c, addr + 1, val >> 8);
}

static inline
uint16_t i8080_get_hl(i8080 *const c)
{
	return (c->h << 8) | c->l;
}

static inline
uint16_t i8080_get_bc(i8080 *const c)
{
	return (c->b << 8) | c->c;
}

static inline
uint16_t i8080_get_de(i8080 *const c)
{
	return (c->d << 8) | c->e;
}

static inline
void i8080_set_hl(i8080 *const c, uint16_t val)
{
	c->h = val >> 8;
	c->l = val & 0xff;
}

static inline
void i8080_set_bc(i8080 *const c, uint16_t val)
{
	c->b = val >> 8;
	c->c = val & 0xff;
}

static inline
void i8080_set_de(i8080 *const c, uint16_t val)
{
	c->d = val >> 8;
	c->e = val & 0xff;
}

static inline
void i8080_xchg(i8080 *const c)
{
	uint16_t val = i8080_get_de(c);

	i8080_set_de(c,i8080_get_hl(c));
	i8080_set_hl(c, val);
}

static inline
void i8080_xthl(i8080 *const c)
{
	uint16_t val = i8080_read_word(c, c->sp);

	i8080_write_word(c, c->sp, i8080_get_hl(c));
	i8080_set_hl(c, val);
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
	uint16_t addr = i8080_get_hl(c);
	uint8_t val = i8080_inr(c, c->read_byte(c, addr));
	c->write_byte(c, addr, val);
}

static inline
uint8_t i8080_get_dec(i8080 *const c, uint8_t val)
{
	uint8_t result = val - 1;
	c->hf = !((result & 0xf) == 0xf);
	i8080_set_zsp(c, val);
	return result;
}

static inline
void i8080_get_decm(i8080 *const c)
{
	uint16_t addr = i8080_get_hl(c);
	uint8_t val = i8080_get_dec(c, c->read_byte(c, addr));
	c->write_byte(c, addr, val);
}

static inline
void i8080_inx_bc(i8080 *const c)
{
	uint16_t val = i8080_get_bc(c) + 1;
	c->b = val >> 8;
	c->c = val & 0xff;
}

static inline
void i8080_inx_de(i8080 *const c)
{
	uint16_t val = i8080_get_de(c) + 1;
	c->d = val >> 8;
	c->e = val & 0xff;
}

static inline
void i8080_inx_hl(i8080 *const c)
{
	uint16_t val = i8080_get_hl(c) + 1;
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
	uint16_t val = i8080_get_bc(c) - 1;
	c->b = val >> 8;
	c->c = val & 0xff;
}

static inline
void i8080_dcx_de(i8080 *const c)
{
	uint16_t val = i8080_get_de(c) - 1;
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
	val += i8080_get_hl(c);
	 // how to set cf ??? i don't know not yet...
	c->cf = 0;	
	c->h = val >> 8;
	c->l = val & 0xff;
}

static inline
void i8080_dcx_hl(i8080 *const c)
{
	uint16_t val = i8080_get_hl(c) - 1;
	c->h = val >> 8;
	c->l = val & 0xff;
}

static inline
void i8080_dcx_sp(i8080 *const c)
{
	c->sp -= 1;
}

static inline
void i8080_jmp(i8080 *const c, uint16_t addr)
{
	c->pc = addr;
}

static inline
void i8080_cond_jmp(i8080 *const c, bool condition)
{
	uint16_t addr = i8080_next_word(c);
	if (condition)
		i8080_jmp(c, addr);	
}

static inline
void i8080_push(i8080 *const c, uint16_t val)
{
	c->sp -= 2;
	/* before
	  low
		val 1 <- c->sp
		val 0
			
	  high	
	*/
	/* after
	  low
		val 2 <- c->sp
		val 1 
		val 0
			
	  high	
	*/

	i8080_write_word(c, c->sp, val);
}

static inline
void i8080_push_psw(i8080 *const c)
{
	uint8_t status = 0;
	status |= c->sf << 7;
	status |= c->zf << 6;
	status |= c->hf << 4;
	status |= c->pf << 2;
	status |= c->cf << 0;
	i8080_push(c, (c->a << 8) | status);	
}

static inline
uint16_t i8080_pop(i8080 *const c)
{
	uint16_t val = i8080_read_word(c, c->sp);
	c->sp += 2;
	return (val);	
}

static inline
void i8080_pop_psw(i8080 *const c)
{
	uint16_t status = i8080_pop(c);
	uint8_t flags = status & 0xff;
	c->a = status >> 8;
	c->sf = (flags >> 7) & 0x01;
	c->zf = (flags >> 6) & 0x01;
	c->hf = (flags >> 4) & 0x01;
	c->pf = (flags >> 2) & 0x01;
	c->cf = (flags >> 0) & 0x01;
	c->sp += 2;
}

static inline
void i8080_call(i8080 *const c, uint16_t addr)
{
	i8080_push(c, c->pc);
	i8080_jmp(c, addr);
}

static inline
void i8080_return(i8080 *const c)
{
	c->pc = i8080_pop(c);
}

static inline
void i8080_cond_return(i8080 *const c, bool cond)
{
	if (!cond)
		return;
	i8080_return(c);
}

void i8080_exec(i8080 *const c, uint8_t opcode)
{
	c->cyc += OPCODES_CYCLES[opcode];

	switch (opcode) 
	{
		// MOV A, R
		case 0x78: c->a = c->b; break;
		case 0x79: c->a = c->c; break;
		case 0x7a: c->a = c->d; break;
		case 0x7b: c->a = c->e; break;
		case 0x7c: c->a = c->h; break;
		case 0x7d: c->a = c->l; break;
		case 0x7f: c->a = c->a; break;
		
		// MOV B, R
		case 0x40: c->b = c->b; break;
		case 0x41: c->b = c->c; break;
		case 0x42: c->b = c->d; break;
		case 0x43: c->b = c->e; break;
		case 0x44: c->b = c->h; break;
		case 0x45: c->b = c->l; break;
		case 0x47: c->b = c->a; break;
	
		// MOV C R	
		case 0x48: c->c = c->b; break;
		case 0x49: c->c = c->c; break;
		case 0x4a: c->c = c->d; break;
		case 0x4b: c->c = c->e; break;
		case 0x4c: c->c = c->h; break;
		case 0x4d: c->c = c->l; break;
		case 0x4f: c->c = c->a; break;
		
		// MOV D R
		case 0x50: c->d = c->b; break;
		case 0x51: c->d = c->c; break;
		case 0x52: c->d = c->d; break;
		case 0x53: c->d = c->e; break;
		case 0x54: c->d = c->h; break;
		case 0x57: c->d = c->a; break;

		// MOV E R
		case 0x58: c->e = c->b; break;
		case 0x59: c->e = c->c; break;
		case 0x5a: c->e = c->d; break;
		case 0x5b: c->e = c->e; break;
		case 0x5c: c->e = c->h; break;
		case 0x5f: c->e = c->a; break;

		// MOV H R
		case 0x60: c->h = c->b; break;
		case 0x61: c->h = c->c; break;
		case 0x62: c->h = c->d; break;
		case 0x63: c->h = c->e; break;
		case 0x64: c->h = c->h; break;
		case 0x67: c->h = c->a; break;

		// MOV L R
		case 0x68: c->l = c->b; break;
		case 0x69: c->l = c->c; break;
		case 0x6a: c->l = c->d; break;
		case 0x6b: c->l = c->e; break;
		case 0x6c: c->l = c->h; break;
		case 0x6f: c->l = c->a; break;

		// MOV R, M
		case 0x7e: c->a = c->read_byte(c, i8080_get_hl(c)); break;
		case 0x46: c->b = c->read_byte(c, i8080_get_hl(c)); break;
		case 0x4e: c->c = c->read_byte(c, i8080_get_hl(c)); break;
		case 0x56: c->d = c->read_byte(c, i8080_get_hl(c)); break;
		case 0x5e: c->e = c->read_byte(c, i8080_get_hl(c)); break;
		case 0x66: c->h = c->read_byte(c, i8080_get_hl(c)); break;
		case 0x6e: c->l = c->read_byte(c, i8080_get_hl(c)); break;
		
		// MOV M, R
		case 0x70: c->write_byte(c, i8080_get_hl(c), c->b); break;		
		case 0x71: c->write_byte(c, i8080_get_hl(c), c->c); break;		
		case 0x72: c->write_byte(c, i8080_get_hl(c), c->d); break;		
		case 0x73: c->write_byte(c, i8080_get_hl(c), c->e); break;		
		case 0x74: c->write_byte(c, i8080_get_hl(c), c->h); break;		
		case 0x75: c->write_byte(c, i8080_get_hl(c), c->l); break;		
		case 0x77: c->write_byte(c, i8080_get_hl(c), c->a); break;		

		// LXI
		case 0x01: i8080_set_bc(c, i8080_next_word(c)); break;
		case 0x11: i8080_set_de(c, i8080_next_word(c)); break;
		case 0x21: i8080_set_hl(c, i8080_next_word(c)); break;
		case 0x31: c->sp = i8080_next_word(c);
		
		// LDA
		case 0x0a: c->a = c->read_byte(c, i8080_get_bc(c)); break;
		case 0x1a: c->a = c->read_byte(c, i8080_get_de(c)); break;
		case 0x3a: c->a = c->read_byte(c, i8080_next_word(c)); break;

		// STAX
		case 0x02: c->write_byte(c, i8080_get_bc(c), c->a); break;
		case 0x12: c->write_byte(c, i8080_get_de(c), c->a); break;

		// STA
		case 0x32: c->write_byte(c, i8080_next_word(c), c->a); break;

		// LHLD M
		case 0x2a: 
			i8080_set_hl(c, i8080_read_word(c,
						i8080_next_word(c)));
			break;
		
		// SHLD M
		case 0x22:
			i8080_write_word(c, i8080_next_word(c) ,i8080_get_hl(c));
			break;
		
		// XCHG:
		case 0xeb: i8080_xchg(c); break;

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
			i8080_add(c, &c->a, c->read_byte(c, i8080_get_hl(c)), c->cf);
			break;		
		// ADD M
		case 0x86: i8080_add(c, &c->a, c->read_byte(c, i8080_get_hl(c)), 0);
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
			i8080_sub(c, &c->a, c->read_byte(c, i8080_get_hl(c)), 0);
			break;	
		// SBB M
		case 0x9e:
			i8080_sub(c, &c->a, c->read_byte(c, i8080_get_hl(c)), c->cf);
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
		case 0x05: c->b = i8080_get_dec(c, c->b); break;
		case 0x15: c->d = i8080_get_dec(c, c->d); break;
		case 0x25: c->h = i8080_get_dec(c, c->h); break;
		case 0x0d: c->c = i8080_get_dec(c, c->c); break;
		case 0x1d: c->e = i8080_get_dec(c, c->e); break;
		case 0x2d: c->l = i8080_get_dec(c, c->l); break;
		case 0x3d: c->a = i8080_get_dec(c, c->a); break;
		// INR M
		case 0x34: i8080_inrm(c); break;
		// DCR M
		case 0x35: i8080_get_decm(c); break;
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
		case 0x09: i8080_dad(c, i8080_get_bc(c)); break;
		case 0x19: i8080_dad(c, i8080_get_de(c)); break;
		case 0x29: i8080_dad(c, i8080_get_hl(c)); break;
		case 0x39: i8080_dad(c, c->sp); break;

		// PUSH
		case 0xc5: i8080_push(c, i8080_get_bc(c)); break;
		case 0xd5: i8080_push(c, i8080_get_de(c)); break;
		case 0xe5: i8080_push(c, i8080_get_hl(c)); break;	
		case 0xf5: i8080_push_psw(c); break;

		// POP
		case 0xc1: i8080_set_bc(c, i8080_pop(c)); break;
		case 0xd1: i8080_set_de(c, i8080_pop(c)); break;
		case 0xe1: i8080_set_hl(c, i8080_pop(c)); break;
		case 0xf1: i8080_pop_psw(c); break;

		// XTHL
		case 0xe3: i8080_xthl(c); break;
		// SPHL
		case 0xf9: c->sp = i8080_get_hl(c);

		// IN PORT 
		case 0xdb: c->a = c->port_in(c, c->read_byte(c, c->pc++)); break; 
		// OUT PORT
		case 0xd3: c->port_out(c, c->read_byte(c, c->pc++), c->a); break;
		// HLT
		case 0x76: c->halted = 1; break;

		// JMP:
		case 0xc3: i8080_jmp(c, i8080_next_word(c)); break;
		// ZERO FLAG
		case 0xc2: i8080_cond_jmp(c, c->zf == 0); break;
		case 0xca: i8080_cond_jmp(c, c->zf == 1); break;
		// CARRY FLAG
		case 0xd2: i8080_cond_jmp(c, c->cf == 0); break;
		case 0xda: i8080_cond_jmp(c, c->cf == 1); break;
		// PARITIY FLAG
		case 0xe2: i8080_cond_jmp(c, c->pf == 0); break;
		case 0xea: i8080_cond_jmp(c, c->pf == 1); break;
		// SIGN FLAG
		case 0xf2: i8080_cond_jmp(c, c->sf == 0); break;
		case 0xfa: i8080_cond_jmp(c, c->sf == 1); break;
		
		// CALL
		case 0xcd:
		case 0xdd:
		case 0xed:
		case 0xfd: 
			i8080_call(c, i8080_next_word(c)); break;
		
		// RST (RESTART)
		case 0xc7: i8080_call(c, 0x00); break;
		case 0xcf: i8080_call(c, 0x08); break;
		case 0xd7: i8080_call(c, 0x10); break;
		case 0xdf: i8080_call(c, 0x18); break;
		case 0xe7: i8080_call(c, 0x20); break;
		case 0xef: i8080_call(c, 0x28); break;
		case 0xf7: i8080_call(c, 0x30); break;
		case 0xff: i8080_call(c, 0x38); break;

		// RET
		case 0xc9:
		case 0xd9:
			i8080_return(c); break;
		// ZERO FLAG
		case 0xc0: i8080_cond_return(c, c->zf == 0); break;
		case 0xc8: i8080_cond_return(c, c->zf == 1); break;
		// CARRY FLAG
		case 0xd0: i8080_cond_return(c, c->cf == 0); break;
		case 0xd8: i8080_cond_return(c, c->cf == 1); break;
		// PARITIY FLAG
		case 0xe0: i8080_cond_return(c, c->pf == 0); break;
		case 0xe8: i8080_cond_return(c, c->pf == 1); break;
		// SIGN FLAG
		case 0xf0: i8080_cond_return(c, c->sf == 0); break;
		case 0xf8: i8080_cond_return(c, c->sf == 1); break;
		
		// PCHL
		case 0xe9: c->pc = i8080_get_hl(c);  break;

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
