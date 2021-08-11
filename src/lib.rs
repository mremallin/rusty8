pub struct Chip8Instance {
	ram: [u8; 0x1000],
	v_regs: [u8; 16],
	i_reg: u16,
	pc: u16,
	stack_ptr: u16,
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


	pub fn clear_display(&mut self) {
		self.vram.iter_mut().for_each(|m| *m = 0);
	}
}

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

	#[test]
	fn opc_00e0() {
		let mut c8i = Chip8Instance::default();

		c8i.vram.iter_mut().for_each(|m| *m = 0xff);

		c8i.clear_display();

		for (i, elem) in c8i.vram.iter().enumerate() {
			assert_eq!(*elem, 0, "VRAM was not cleared at byte {}", i);
		}

	}
}