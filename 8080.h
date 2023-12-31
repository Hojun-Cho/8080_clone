#ifndef I8080_H
#define I8080_H

#include <stdint.h>
#include <stdbool.h>

static const uint8_t OPCODES_CYCLES[256] = {
//	0  1   2   3   4   5   6   7   8  9   A   B   C   D   E  F
	4, 10, 7,  5,  5,  5,  7,  4,  4, 10, 7,  5,  5,  5,  7, 4,  // 0
	4, 10, 7,  5,  5,  5,  7,  4,  4, 10, 7,  5,  5,  5,  7, 4,  // 1
	4, 10, 16, 5,  5,  5,  7,  4,  4, 10, 16, 5,  5,  5,  7, 4,  // 2
	4, 10, 13, 5,  10, 10, 10, 4,  4, 10, 13, 5,  5,  5,  7, 4,  // 3
	5, 5,  5,  5,  5,  5,  7,  5,  5, 5,  5,  5,  5,  5,  7, 5,  // 4
	5, 5,  5,  5,  5,  5,  7,  5,  5, 5,  5,  5,  5,  5,  7, 5,  // 5
	5, 5,  5,  5,  5,  5,  7,  5,  5, 5,  5,  5,  5,  5,  7, 5,  // 6
	7, 7,  7,  7,  7,  7,  7,  7,  5, 5,  5,  5,  5,  5,  7, 5,  // 7
	4, 4,  4,  4,  4,  4,  7,  4,  4, 4,  4,  4,  4,  4,  7, 4,  // 8
	4, 4,  4,  4,  4,  4,  7,  4,  4, 4,  4,  4,  4,  4,  7, 4,  // 9
	4, 4,  4,  4,  4,  4,  7,  4,  4, 4,  4,  4,  4,  4,  7, 4,  // A
	4, 4,  4,  4,  4,  4,  7,  4,  4, 4,  4,  4,  4,  4,  7, 4,  // B
	5, 10, 10, 10, 11, 11, 7,  11, 5, 10, 10, 10, 11, 17, 7, 11, // C
	5, 10, 10, 10, 11, 11, 7,  11, 5, 10, 10, 10, 11, 17, 7, 11, // D
	5, 10, 10, 18, 11, 11, 7,  11, 5, 5,  10, 4,  11, 17, 7, 11, // E
	5, 10, 10, 4,  11, 11, 7,  11, 5, 5,  10, 4,  11, 17, 7, 11  // F
};

static const char* DISASSEMBLE_TABLE[] = {
	"nop", "lxi b,#", "stax b", "inx b",
	"inr b", "dcr b", "mvi b,#", "rlc", "ill", "dad b", "ldax b", "dcx b",
	"inr c", "dcr c", "mvi c,#", "rrc", "ill", "lxi d,#", "stax d", "inx d",
	"inr d", "dcr d", "mvi d,#", "ral", "ill", "dad d", "ldax d", "dcx d",
	"inr e", "dcr e", "mvi e,#", "rar", "ill", "lxi h,#", "shld", "inx h",
	"inr h", "dcr h", "mvi h,#", "daa", "ill", "dad h", "lhld", "dcx h",
	"inr l", "dcr l", "mvi l,#", "cma", "ill", "lxi sp,#", "sta $", "inx sp",
	"inr M", "dcr M", "mvi M,#", "stc", "ill", "dad sp", "lda $", "dcx sp",
	"inr a", "dcr a", "mvi a,#", "cmc", "mov b,b", "mov b,c", "mov b,d",
	"mov b,e", "mov b,h", "mov b,l", "mov b,M", "mov b,a", "mov c,b", "mov c,c",
	"mov c,d", "mov c,e", "mov c,h", "mov c,l", "mov c,M", "mov c,a", "mov d,b",
	"mov d,c", "mov d,d", "mov d,e", "mov d,h", "mov d,l", "mov d,M", "mov d,a",
	"mov e,b", "mov e,c", "mov e,d", "mov e,e", "mov e,h", "mov e,l", "mov e,M",
	"mov e,a", "mov h,b", "mov h,c", "mov h,d", "mov h,e", "mov h,h", "mov h,l",
	"mov h,M", "mov h,a", "mov l,b", "mov l,c", "mov l,d", "mov l,e", "mov l,h",
	"mov l,l", "mov l,M", "mov l,a", "mov M,b", "mov M,c", "mov M,d", "mov M,e",
	"mov M,h", "mov M,l", "hlt", "mov M,a", "mov a,b", "mov a,c", "mov a,d",
	"mov a,e", "mov a,h", "mov a,l", "mov a,M", "mov a,a", "add b", "add c",
	"add d", "add e", "add h", "add l", "add M", "add a", "adc b", "adc c",
	"adc d", "adc e", "adc h", "adc l", "adc M", "adc a", "sub b", "sub c",
	"sub d", "sub e", "sub h", "sub l", "sub M", "sub a", "sbb b", "sbb c",
	"sbb d", "sbb e", "sbb h", "sbb l", "sbb M", "sbb a", "ana b", "ana c",
	"ana d", "ana e", "ana h", "ana l", "ana M", "ana a", "xra b", "xra c",
	"xra d", "xra e", "xra h", "xra l", "xra M", "xra a", "ora b", "ora c",
	"ora d", "ora e", "ora h", "ora l", "ora M", "ora a", "cmp b", "cmp c",
	"cmp d", "cmp e", "cmp h", "cmp l", "cmp M", "cmp a", "rnz", "pop b",
	"jnz $", "jmp $", "cnz $", "push b", "adi #", "rst 0", "rz", "ret", "jz $",
	"ill", "cz $", "call $", "aci #", "rst 1", "rnc", "pop d", "jnc $", "out p",
	"cnc $", "push d", "sui #", "rst 2", "rc", "ill", "jc $", "in p", "cc $",
	"ill", "sbi #", "rst 3", "rpo", "pop h", "jpo $", "xthl", "cpo $", "push h",
	"ani #", "rst 4", "rpe", "pchl", "jpe $", "xchg", "cpe $", "ill", "xri #",
	"rst 5", "rp", "pop psw", "jp $", "di", "cp $", "push psw", "ori #",
	"rst 6", "rm", "sphl", "jm $", "ei", "cm $", "ill", "cpi #", "rst 7"
};

typedef struct cpu_i8080 i8080;
struct cpu_i8080
{
	uint8_t (*read_byte)(i8080*, uint16_t); // read from memroy
	void (*write_byte)(i8080*, uint16_t, uint8_t); // write to memory
	uint8_t (*port_in)(i8080*, uint8_t); // // read from port
	void (*port_out)(i8080*, uint8_t, uint8_t); // write to port

	uint32_t cyc;
	uint16_t pc, sp;
	// registers
	uint8_t a, b, c, d, e, h, l; 
	// sign , zero , half carry, parity carry, carry flag, interrupt flip-flop
	bool sf, zf, hf, pf , cf, iff;
	bool halted;
	
	bool interrupt_pending;
	uint8_t interrupt_vector;
	uint8_t interrupt_delay;
};

void i8080_exec(i8080 *const c, uint8_t opcode);


#endif
