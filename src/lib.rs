pub struct Chip8Instance {
	ram: [u8; 0x1000],
	v_regs: [u8; 16],
	i_reg: u16,
	pc: u16,
	stack_ptr: usize,
	paused_for_key: bool,
	vram: [u8; 640 * 320],
}

impl Chip8Instance {
	pub fn new() -> Chip8Instance {
		Chip8Instance {
			ram: [0; 0x1000],
			v_regs: [0; 16],
			i_reg: 0,
			pc: 0,
			stack_ptr: 0,
			paused_for_key: false,
			vram: [0; 640 * 320],
		}
	}

	fn unknown_instruction(&mut self, instruction: u16) {
		println!("Unknown instruction decoded: {:04x}", instruction);
	}

	fn clear_display(&mut self) {
		self.vram.iter_mut().for_each(|m| *m = 0);
	}

	fn stack_pop(&mut self) -> u16 {
		self.stack_ptr += 2;

		let word_pop: u16 = (self.ram[self.stack_ptr] as u16) << 8 | (self.ram[self.stack_ptr + 1] as u16);

		word_pop
	}

	fn match_opcode_0(&mut self, instruction: u16) {
		match instruction {
			0x00e0 => self.clear_display(),
			0x00ee => self.pc = self.stack_pop(),
			_ => self.unknown_instruction(instruction),
		}
	}

	fn is_little_endian() -> bool {
		(47 as u16).to_be() != 47
	}

	pub fn interpret_instruction(&mut self, mut instruction: u16) {
		println!("I1 {:x}", instruction);
		if Chip8Instance::is_little_endian() {
			instruction = instruction.to_be() as u16;
		}

		match (instruction & 0xF000) >> 12 {
			0x0 => self.match_opcode_0(instruction),
			_ => self.unknown_instruction(instruction),
		}
	}
}

/* Default used for UT purposes */
impl Default for Chip8Instance {
	fn default() -> Chip8Instance {
		Chip8Instance {
			ram: [0; 0x1000],
			v_regs: [0; 16],
			i_reg: 0,
			pc: 0,
			stack_ptr: 0,
			paused_for_key: false,
			vram: [0; 640 * 320],
		}
	}
}

#[cfg(test)]
mod chip8_tests {
	use crate::Chip8Instance;

	/* CHIP8 operates with Big-Endian data so for test convenience, handle
	 * the byteswap of the instruction for test readability */
	fn interpret_instruction(c8i: &mut Chip8Instance, instruction: u16) {
		let bswap_instruction = instruction.to_be();
		c8i.interpret_instruction(bswap_instruction);
	}

	#[test]
	fn opc_00e0() {
		let mut c8i = Chip8Instance::default();

		c8i.vram.iter_mut().for_each(|m| *m = 0xff);

		interpret_instruction(&mut c8i, 0x00e0);

		for (i, elem) in c8i.vram.iter().enumerate() {
			assert_eq!(*elem, 0, "VRAM was not cleared at byte {}", i);
		}
	}

	#[test]
	fn opc_00ee() {
		let mut c8i = Chip8Instance::default();

		c8i.stack_ptr = 0xe00;
		c8i.ram[c8i.stack_ptr + 2] = 0xDE;
		c8i.ram[c8i.stack_ptr + 3] = 0xAD;

		interpret_instruction(&mut c8i, 0x00ee);

		assert_eq!(c8i.pc, 0xDEAD, "Got PC {:x}", c8i.pc);
		assert_eq!(c8i.stack_ptr, 0xE02);
	}
}
