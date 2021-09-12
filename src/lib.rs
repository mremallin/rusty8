use rand::prelude::*;
use variant_count::VariantCount;

#[derive(VariantCount)]
enum Chip8Key {
    Key0,
    Key1,
    Key2,
    Key3,
    Key4,
    Key5,
    Key6,
    Key7,
    Key8,
    Key9,
    KeyA,
    KeyB,
    KeyC,
    KeyD,
    KeyE,
    KeyF,
}

pub struct Chip8Instance {
    rng: Box<dyn RngCore>,
    ram: [u8; 0x1000],
    v_regs: [u8; 16],
    i_reg: u16,
    pc: u16,
    stack_ptr: usize,
    vram: [[u8; Chip8Instance::DISPLAY_WIDTH_PIXELS]; Chip8Instance::DISPLAY_HEIGHT_PIXELS],
    keys_pressed: [bool; Chip8Key::VARIANT_COUNT],
    delay_timer: u64,
}

impl Chip8Instance {
    /* Information from https://en.wikipedia.org/wiki/CHIP-8 */
    const PROGRAM_LOAD_ADDR: u16 = 0x200;
    /* 0xEFF is the last valid address in the stack, but because the
     * stack stores 16-bit pointers we start at 0xEFE for alignment and to
     * not overwrite past the stack boundaries */
    const STACK_BASE_ADDR: usize = 0xEFE;
    const NUM_V_REGISTERS: usize = 16;
    const DISPLAY_WIDTH_PIXELS: usize = 64;
    const DISPLAY_HEIGHT_PIXELS: usize = 32;

    fn unknown_instruction(&mut self, instruction: u16) {
        println!("Unknown instruction decoded: {:04x}", instruction);
    }

    fn clear_display(&mut self) {
        self.vram.iter_mut().for_each(|m| *m = [0; 64]);
    }

    fn stack_push(&mut self, val: u16) {
        self.ram[self.stack_ptr] = ((val & 0xFF00) >> 8) as u8;
        self.ram[self.stack_ptr + 1] = (val & 0xFF) as u8;
        self.stack_ptr -= 2;
    }

    fn stack_pop(&mut self) -> u16 {
        self.stack_ptr += 2;

        let word_pop: u16 =
            (self.ram[self.stack_ptr] as u16) << 8 | (self.ram[self.stack_ptr + 1] as u16);

        word_pop
    }

    fn match_opcode_0(&mut self, instruction: u16) {
        match instruction {
            0x00e0 => self.clear_display(),
            0x00ee => self.pc = self.stack_pop(),
            _ => self.unknown_instruction(instruction),
        }
    }

    fn opc_n(instruction: u16) -> u8 {
        (instruction & 0xF) as u8
    }

    fn opc_nn(instruction: u16) -> u8 {
        (instruction & 0xFF) as u8
    }

    fn opc_nnn(instruction: u16) -> u16 {
        instruction & 0x0FFF
    }

    fn opc_regx(instruction: u16) -> usize {
        ((instruction & 0x0F00) >> 8).into()
    }

    fn opc_regy(instruction: u16) -> usize {
        ((instruction & 0x00F0) >> 4).into()
    }

    fn match_opcode_1(&mut self, instruction: u16) {
        self.pc = Chip8Instance::opc_nnn(instruction);
    }

    fn match_opcode_2(&mut self, instruction: u16) {
        self.stack_push(self.pc);
        self.pc = Chip8Instance::opc_nnn(instruction);
    }

    fn match_opcode_3(&mut self, instruction: u16) {
        let vx = Chip8Instance::opc_regx(instruction);
        let val = Chip8Instance::opc_nn(instruction);

        if self.v_regs[vx] == val {
            self.pc += 2;
        }
    }

    fn match_opcode_4(&mut self, instruction: u16) {
        let vx = Chip8Instance::opc_regx(instruction);
        let val = Chip8Instance::opc_nn(instruction);

        if self.v_regs[vx] != val {
            self.pc += 2;
        }
    }

    fn match_opcode_5(&mut self, instruction: u16) {
        let vx = Chip8Instance::opc_regx(instruction);
        let vy = Chip8Instance::opc_regy(instruction);

        if self.v_regs[vx] == self.v_regs[vy] {
            self.pc += 2;
        }
    }

    fn match_opcode_6(&mut self, instruction: u16) {
        let vx = Chip8Instance::opc_regx(instruction);
        let nn = Chip8Instance::opc_nn(instruction);

        self.v_regs[vx] = nn;
    }

    fn match_opcode_7(&mut self, instruction: u16) {
        let vx = Chip8Instance::opc_regx(instruction);
        let nn = Chip8Instance::opc_nn(instruction);

        self.v_regs[vx] = u8::wrapping_add(self.v_regs[vx], nn);
    }

    fn match_opcode_8(&mut self, instruction: u16) {
        match instruction & 0xF {
            /* LD Vx, Vy - Set Vx = Vy. */
            0 => {
                self.v_regs[Chip8Instance::opc_regx(instruction)] =
                    self.v_regs[Chip8Instance::opc_regy(instruction)]
            }
            /* OR Vx, Vy - Set Vx = Vx OR Vy. */
            1 => {
                self.v_regs[Chip8Instance::opc_regx(instruction)] = self.v_regs
                    [Chip8Instance::opc_regx(instruction)]
                    | self.v_regs[Chip8Instance::opc_regy(instruction)]
            }
            /* AND Vx, Vy - Set Vx = Vx AND Vy. */
            2 => {
                self.v_regs[Chip8Instance::opc_regx(instruction)] = self.v_regs
                    [Chip8Instance::opc_regx(instruction)]
                    & self.v_regs[Chip8Instance::opc_regy(instruction)]
            }
            /* XOR Vx, Vy - Set Vx = Vx XOR Vy. */
            3 => {
                self.v_regs[Chip8Instance::opc_regx(instruction)] = self.v_regs
                    [Chip8Instance::opc_regx(instruction)]
                    ^ self.v_regs[Chip8Instance::opc_regy(instruction)]
            }
            /* ADD Vx, Vy - Set Vx = Vx + Vy, set VF = carry. */
            4 => {
                let tmp = u16::wrapping_add(
                    self.v_regs[Chip8Instance::opc_regx(instruction)] as u16,
                    self.v_regs[Chip8Instance::opc_regy(instruction)] as u16,
                );
                self.v_regs[Chip8Instance::opc_regx(instruction)] = (tmp & 0x00ff) as u8;
                self.v_regs[0xf] = ((tmp & 0x100) >> 8) as u8;
            }
            /* SUB Vx, Vy - Set Vx = Vx - Vy, set VF = NOT borrow. */
            5 => {
                if self.v_regs[Chip8Instance::opc_regx(instruction)]
                    > self.v_regs[Chip8Instance::opc_regy(instruction)]
                {
                    self.v_regs[0xf] = 1;
                } else {
                    self.v_regs[0xf] = 0;
                }
                self.v_regs[Chip8Instance::opc_regx(instruction)] = u8::wrapping_sub(
                    self.v_regs[Chip8Instance::opc_regx(instruction)],
                    self.v_regs[Chip8Instance::opc_regy(instruction)],
                );
            }
            /* SHR Vx - Set Vx = Vx SHR 1. */
            6 => {
                self.v_regs[0xf] = self.v_regs[Chip8Instance::opc_regx(instruction)] & 0x1;
                self.v_regs[Chip8Instance::opc_regx(instruction)] =
                    self.v_regs[Chip8Instance::opc_regx(instruction)] >> 1;
            }
            /* SUBN Vx, Vy - Set Vx = Vy - Vx, set VF = NOT borrow. */
            7 => {
                if self.v_regs[Chip8Instance::opc_regy(instruction)]
                    > self.v_regs[Chip8Instance::opc_regx(instruction)]
                {
                    self.v_regs[0xf] = 1;
                } else {
                    self.v_regs[0xf] = 0;
                }
                self.v_regs[Chip8Instance::opc_regx(instruction)] = u8::wrapping_sub(
                    self.v_regs[Chip8Instance::opc_regy(instruction)],
                    self.v_regs[Chip8Instance::opc_regx(instruction)],
                );
            }
            0xE => {
                if (self.v_regs[Chip8Instance::opc_regx(instruction)] & 0x80) != 0 {
                    self.v_regs[0xf] = 1;
                } else {
                    self.v_regs[0xf] = 0;
                }
                self.v_regs[Chip8Instance::opc_regx(instruction)] =
                    self.v_regs[Chip8Instance::opc_regx(instruction)] << 1;
            }
            _ => self.unknown_instruction(instruction),
        }
    }

    fn match_opcode_9(&mut self, instruction: u16) {
        match instruction & 0xF {
            0 => {
                if self.v_regs[Chip8Instance::opc_regx(instruction)]
                    != self.v_regs[Chip8Instance::opc_regy(instruction)]
                {
                    self.pc = u16::wrapping_add(self.pc, 2);
                }
            }
            _ => self.unknown_instruction(instruction),
        }
    }

    fn match_opcode_a(&mut self, instruction: u16) {
        self.i_reg = Chip8Instance::opc_nnn(instruction);
    }

    fn match_opcode_b(&mut self, instruction: u16) {
        self.i_reg = Chip8Instance::opc_nnn(instruction) + self.v_regs[0] as u16;
    }

    fn match_opcode_c(&mut self, instruction: u16) {
        self.v_regs[0] = self.rng.gen::<u8>() & Chip8Instance::opc_nn(instruction);
    }

    fn match_opcode_d(&mut self, instruction: u16) {
        /* DRW Vx, Vy, N
         * Display N-byte sprite starting at memory location I at (Vx, Vy),
         * set VF = collision.
         */
        let vx = self.v_regs[Chip8Instance::opc_regx(instruction)] as usize;
        let mut vy = self.v_regs[Chip8Instance::opc_regy(instruction)] as usize;
        let num_bytes = Chip8Instance::opc_n(instruction);
        let mut sprite_addr = self.i_reg as usize;
        let mut previous_sprite;
        let mut paintbrush;

        /* Start by assuming no pixels will be erased */
        self.v_regs[0xf] = 0;

        for _i in 0..num_bytes {
            previous_sprite = 0;
            /* For each byte of the sprite, the packed byte (1 bit per pixel)
             * needs to be expanded back out into individual bytes */
            for j in 0..8 {
                previous_sprite |= self.vram[vy][vx + j];
            }

            /* Now write a byte of the new sprite to vram */
            for j in 0..8 {
                if (self.ram[sprite_addr] & (1 << (7 - j))) != 0 {
                    paintbrush = 0xff;
                } else {
                    paintbrush = 0x0;
                }

                self.vram[vy][vx + j] = paintbrush ^ self.vram[vy][vx + j];
            }

            /* The following sets VF without a branch, only a comparison.
             * Let's say we have the following byte in VRAM to start
             * 0x8A (10001010). If the new byte coming in clears any of
             * those bits, the resulting byte in VRAM will always be
             * less than the previous value. We can use that comparison
             * to set the bit in VF indicating that a pixel was erased.
             */
            self.v_regs[0xf] |= ((previous_sprite > self.ram[sprite_addr]) as u8) & 0x1;

            /* Move to next row on the screen */
            sprite_addr += 1;
            vy += 1;
            vy = vy % Chip8Instance::DISPLAY_HEIGHT_PIXELS;
        }
    }

    fn match_opcode_e(&mut self, instruction: u16) {
        let op = Chip8Instance::opc_nn(instruction);

        match op {
            0x9e =>
            /* SKP Vx */
            {
                if self.keys_pressed[self.v_regs[Chip8Instance::opc_regx(instruction)] as usize] {
                    self.pc += 2
                }
            }
            0xa1 =>
            /* SKNP Vx */
            {
                if !self.keys_pressed[self.v_regs[Chip8Instance::opc_regx(instruction)] as usize] {
                    self.pc += 2
                }
            }
            _ => self.unknown_instruction(instruction),
        }
    }

    fn match_opcode_f(&mut self, instruction: u16) {
        let op = Chip8Instance::opc_nn(instruction);

        match op {
            0x7 =>
            /* LD Vx, DT */
            {
                self.v_regs[Chip8Instance::opc_regx(instruction)] = self.delay_timer as u8
            }
            _ => self.unknown_instruction(instruction),
        }
    }

    fn is_little_endian() -> bool {
        (47 as u16).to_be() != 47
    }

    pub fn interpret_instruction(&mut self, mut instruction: u16) {
        if Chip8Instance::is_little_endian() {
            instruction = instruction.to_be() as u16;
        }

        let instruction_type = (instruction & 0xF000) >> 12;
        match instruction_type {
            0x0 => self.match_opcode_0(instruction),
            0x1 => self.match_opcode_1(instruction),
            0x2 => self.match_opcode_2(instruction),
            0x3 => self.match_opcode_3(instruction),
            0x4 => self.match_opcode_4(instruction),
            0x5 => self.match_opcode_5(instruction),
            0x6 => self.match_opcode_6(instruction),
            0x7 => self.match_opcode_7(instruction),
            0x8 => self.match_opcode_8(instruction),
            0x9 => self.match_opcode_9(instruction),
            0xa => self.match_opcode_a(instruction),
            0xb => self.match_opcode_b(instruction),
            0xc => self.match_opcode_c(instruction),
            0xd => self.match_opcode_d(instruction),
            0xe => self.match_opcode_e(instruction),
            0xf => self.match_opcode_f(instruction),
            /* This is not technically possible as the match is only on the first
             * nibble of the instruction but is required to suppress a compiler error
             * that not all matches are covered. */
            _ => self.unknown_instruction(instruction),
        }
    }
}

/* Default used for UT purposes */
impl Default for Chip8Instance {
    fn default() -> Chip8Instance {
        Chip8Instance {
            rng: Box::new(thread_rng()),
            ram: [0; 0x1000],
            v_regs: [0; Chip8Instance::NUM_V_REGISTERS],
            i_reg: 0,
            pc: Chip8Instance::PROGRAM_LOAD_ADDR,
            stack_ptr: Chip8Instance::STACK_BASE_ADDR,
            vram: [[0; Chip8Instance::DISPLAY_WIDTH_PIXELS]; Chip8Instance::DISPLAY_HEIGHT_PIXELS],
            keys_pressed: [false; Chip8Key::VARIANT_COUNT],
            delay_timer: 0,
        }
    }
}

#[cfg(test)]
mod chip8_tests {
    use crate::Chip8Instance;
    use crate::Chip8Key;
    use rand::rngs::mock::StepRng;

    /* CHIP8 operates with Big-Endian data so for test convenience, handle
     * the byteswap of the instruction for test readability */
    fn interpret_instruction(c8i: &mut Chip8Instance, instruction: u16) {
        if Chip8Instance::is_little_endian() {
            let bswap_instruction = instruction.to_be();
            c8i.interpret_instruction(bswap_instruction);
        } else {
            c8i.interpret_instruction(instruction);
        }
    }

    fn build_nnn_opc(opc: u8, nnn: u16) -> u16 {
        (((opc & 0xF) as u16) << 12 | ((nnn & 0xFFF) as u16)) as u16
    }

    fn build_xnn_opc(opc: u8, x: u8, nn: u8) -> u16 {
        (((opc & 0xF) as u16) << 12 | ((x & 0xF) as u16) << 8 | (nn as u16)).into()
    }

    fn build_xyn_opc(opc: u8, x: u8, y: u8, n: u8) -> u16 {
        ((((opc & 0xF) as u16) << 12 | ((x & 0xF) as u16) << 8 | ((y & 0xF) as u16) << 4)
            | ((n & 0xf) as u16))
            .into()
    }

    #[test]
    /* Clear the display */
    fn opc_00e0() {
        let mut c8i = Chip8Instance::default();

        c8i.vram
            .iter_mut()
            .for_each(|m| *m = [0xff; Chip8Instance::DISPLAY_WIDTH_PIXELS]);

        interpret_instruction(&mut c8i, 0x00e0);

        for (i, row) in c8i.vram.iter().enumerate() {
            for (j, column) in row.iter().enumerate() {
                assert_eq!(*column, 0, "VRAM was not cleared at {}, {}", i, j);
            }
        }
    }

    #[test]
    /* Returns from a subroutine */
    fn opc_00ee() {
        let mut c8i = Chip8Instance::default();

        c8i.stack_ptr = 0xe00;
        c8i.ram[c8i.stack_ptr + 2] = 0xDE;
        c8i.ram[c8i.stack_ptr + 3] = 0xAD;

        interpret_instruction(&mut c8i, 0x00ee);

        assert_eq!(c8i.pc, 0xDEAD);
        assert_eq!(c8i.stack_ptr, 0xE02);
    }

    #[test]
    /* Jumps to address 0xNNN */
    fn opc_1nnn() {
        let mut c8i = Chip8Instance::default();

        for i in 0x1000..0x2000 {
            interpret_instruction(&mut c8i, i);
            assert_eq!(c8i.pc, i - 0x1000);
        }
    }

    #[test]
    /* Calls subroutine at 0xNNN */
    fn opc_2nnn() {
        let mut c8i = Chip8Instance::default();

        for i in 0x2000..0x3000 {
            interpret_instruction(&mut c8i, i);

            /* Push current PC on the stack */
            assert_eq!(c8i.stack_ptr, Chip8Instance::STACK_BASE_ADDR - 2);
            /* Load PC with 3 nibbles of the op */
            assert_eq!(c8i.pc, Chip8Instance::opc_nnn(i));
            /* Return from the subroutine to cleanup */
            interpret_instruction(&mut c8i, 0x00EE);
        }
    }

    #[test]
    /* Skips the next instruction if VX equals NN.
     * (Usually the next instruction is a jump to skip a code block) */
    fn opc_3xnn_skip() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            let op = build_xnn_opc(3, i as u8, 00);
            interpret_instruction(&mut c8i, op);

            assert_eq!(c8i.pc, Chip8Instance::PROGRAM_LOAD_ADDR + 2);
            c8i.pc = Chip8Instance::PROGRAM_LOAD_ADDR;
        }
    }

    #[test]
    fn opc_3xnn_noskip() {
        let mut c8i = Chip8Instance::default();

        c8i.v_regs.iter_mut().for_each(|m| *m = 0xff);

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            let op = build_xnn_opc(3, i as u8, 00);
            interpret_instruction(&mut c8i, op);

            assert_eq!(c8i.pc, Chip8Instance::PROGRAM_LOAD_ADDR);
            c8i.pc = Chip8Instance::PROGRAM_LOAD_ADDR;
        }
    }

    #[test]
    /* Skips the next instruction if VX doesn't equal NN.
     * (Usually the next instruction is a jump to skip a code block)  */
    fn opc_4xnn_skip() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            let op = build_xnn_opc(4, i as u8, 0xDE);
            interpret_instruction(&mut c8i, op);

            assert_eq!(c8i.pc, Chip8Instance::PROGRAM_LOAD_ADDR + 2);
            c8i.pc = Chip8Instance::PROGRAM_LOAD_ADDR;
        }
    }

    #[test]
    fn opc_4xnn_noskip() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            let op = build_xnn_opc(4, i as u8, 0);
            interpret_instruction(&mut c8i, op);

            assert_eq!(c8i.pc, Chip8Instance::PROGRAM_LOAD_ADDR);
            c8i.pc = Chip8Instance::PROGRAM_LOAD_ADDR;
        }
    }

    #[test]
    /* Skips the next instruction if VX equals VY.
     * (Usually the next instruction is a jump to skip a code block)  */
    fn opc_5xy0_skip() {
        let mut c8i = Chip8Instance::default();

        /* Test each register behaves the same */
        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            for j in 0..Chip8Instance::NUM_V_REGISTERS {
                let op = build_xyn_opc(5, i as u8, j as u8, 0);
                interpret_instruction(&mut c8i, op);

                /* Match, so PC should be incremented again. */
                assert_eq!(c8i.pc, Chip8Instance::PROGRAM_LOAD_ADDR + 2);

                /* Reset for next instruction */
                c8i.pc = Chip8Instance::PROGRAM_LOAD_ADDR;
            }
        }
    }

    #[test]
    /* Skips the next instruction if VX equals VY.
     * (Usually the next instruction is a jump to skip a code block)  */
    fn opc_5xy0_noskip() {
        let mut c8i = Chip8Instance::default();

        /* Test each register behaves the same */
        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            for j in 0..Chip8Instance::NUM_V_REGISTERS {
                let op = build_xyn_opc(5, i as u8, j as u8, 0);

                /* Ensure that VI != VJ */
                if i == j {
                    interpret_instruction(&mut c8i, op);
                    /* X == Y so the contents of the register is guaranteed to
                     * be identical */
                    assert_eq!(c8i.pc, Chip8Instance::PROGRAM_LOAD_ADDR + 2);
                } else {
                    c8i.v_regs[i] = 0x1;
                    c8i.v_regs[j] = 0x2;
                    interpret_instruction(&mut c8i, op);
                    /* No match, so PC should not be incremented. */
                    assert_eq!(c8i.pc, Chip8Instance::PROGRAM_LOAD_ADDR);
                }

                c8i.pc = Chip8Instance::PROGRAM_LOAD_ADDR;
            }
        }
    }

    #[test]
    /* Sets Vx to NN */
    fn opc_6xnn() {
        let mut c8i = Chip8Instance::default();

        for i in 0x6000..0x7000 {
            interpret_instruction(&mut c8i, i);
            assert_eq!(
                c8i.v_regs[Chip8Instance::opc_regx(i)],
                Chip8Instance::opc_nn(i)
            );
        }
    }

    #[test]
    /* Adds NN to VX. (Carry flag is not changed)  */
    fn opc_7xnn() {
        let mut c8i = Chip8Instance::default();

        for i in 0x7000..0x8000 {
            c8i.v_regs[Chip8Instance::opc_regx(i)] = 5;

            interpret_instruction(&mut c8i, i);
            assert_eq!(
                c8i.v_regs[Chip8Instance::opc_regx(i)],
                u8::wrapping_add(Chip8Instance::opc_nn(i), 5)
            );
            if Chip8Instance::opc_regx(i) != 0xF {
                assert_eq!(c8i.v_regs[0xF], 0);
            }
        }
    }

    #[test]
    /* Sets VX to the value of VY. */
    fn opc_8xy0() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            for j in 0..Chip8Instance::NUM_V_REGISTERS {
                let op = build_xyn_opc(8, i as u8, j as u8, 0);

                /* Set destination with a known value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, i as u8, 5));
                /* Set source to test value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, j as u8, 0xa0));
                /* Set VX to VY */
                interpret_instruction(&mut c8i, op);
                assert_eq!(c8i.v_regs[i], c8i.v_regs[j]);
            }
        }
    }

    #[test]
    /* Sets VX to (VX or VY). (Bitwise OR operation) */
    fn opc_8xy1() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            for j in 0..Chip8Instance::NUM_V_REGISTERS {
                let op = build_xyn_opc(8, i as u8, j as u8, 1);

                /* Set destination with a known value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, i as u8, 5));
                /* Set source to test value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, j as u8, 0xa0));
                /* Set Vx = Vx | Vy */
                interpret_instruction(&mut c8i, op);

                if i != j {
                    assert_eq!(c8i.v_regs[i], 0xa5);
                } else {
                    assert_eq!(c8i.v_regs[i], 0xa0);
                }
            }
        }
    }

    #[test]
    /* Sets VX to (VX and VY). (Bitwise AND operation) */
    fn opc_8xy2() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            for j in 0..Chip8Instance::NUM_V_REGISTERS {
                let op = build_xyn_opc(8, i as u8, j as u8, 2);

                /* Set destination with a known value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, i as u8, 5));
                /* Set source to test value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, j as u8, 0xa0));
                /* Set Vx = Vx & Vy */
                interpret_instruction(&mut c8i, op);

                if i == j {
                    assert_eq!(c8i.v_regs[i], 0xa0);
                } else {
                    assert_eq!(c8i.v_regs[i], 0x0);
                }
            }
        }
    }

    #[test]
    /* Sets VX to (VX xor VY). (Bitwise XOR operation) */
    fn opc_8xy3() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            for j in 0..Chip8Instance::NUM_V_REGISTERS {
                let op = build_xyn_opc(8, i as u8, j as u8, 3);

                /* Set destination with a known value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, i as u8, 0xb5));
                /* Set source to test value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, j as u8, 0xa0));
                /* Set Vx = Vx & Vy */
                interpret_instruction(&mut c8i, op);

                if i == j {
                    assert_eq!(c8i.v_regs[i], 0x0);
                } else {
                    assert_eq!(c8i.v_regs[i], 0x15);
                }
            }
        }
    }

    #[test]
    /* Adds VY to VX. VF is set to 1 when there's a carry,
     * and to 0 when there isn't.   */
    fn opc_8xy4_no_carry() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            for j in 0..Chip8Instance::NUM_V_REGISTERS {
                let op = build_xyn_opc(8, i as u8, j as u8, 4);

                /* Set destination with a known value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, i as u8, 5));
                /* Set source to test value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, j as u8, 0x10));
                /* Set Vx = Vx & Vy */
                interpret_instruction(&mut c8i, op);

                if i == 0xF {
                    assert_eq!(c8i.v_regs[i], 0);
                } else if i == j {
                    assert_eq!(c8i.v_regs[i], 0x20);
                    assert_eq!(c8i.v_regs[0xf], 0);
                } else {
                    assert_eq!(c8i.v_regs[i], 0x15);
                    assert_eq!(c8i.v_regs[0xf], 0);
                }
            }
        }
    }

    #[test]
    /* Adds VY to VX. VF is set to 1 when there's a carry,
     * and to 0 when there isn't.   */
    fn opc_8xy4_carry() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            for j in 0..Chip8Instance::NUM_V_REGISTERS {
                let op = build_xyn_opc(8, i as u8, j as u8, 4);

                /* Set destination with a known value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, i as u8, 0xff));
                /* Set source to test value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, j as u8, 0xaf));
                /* Set Vx = Vx & Vy */
                interpret_instruction(&mut c8i, op);

                if i == 0xf {
                    assert_eq!(c8i.v_regs[i], 1);
                } else if i == j {
                    assert_eq!(c8i.v_regs[i], 0x5e);
                    assert_eq!(c8i.v_regs[0xf], 1);
                } else {
                    assert_eq!(c8i.v_regs[i], 0xae);
                    assert_eq!(c8i.v_regs[0xf], 1);
                }
            }
        }
    }

    #[test]
    /* Subtract VY from VX. VF is set to 1 when Vx > Vy, and
     * 0 otherwise. */
    fn opc_8xy5() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            for j in 0..Chip8Instance::NUM_V_REGISTERS {
                let op = build_xyn_opc(8, i as u8, j as u8, 5);

                /* Set destination with a known value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, i as u8, 0x10));
                /* Set source to test value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, j as u8, 5));
                /* Set Vx = Vx & Vy */
                interpret_instruction(&mut c8i, op);

                /* Different from the Add operation, the borrow flag is set
                 * first and then subtraction is performed. This means that
                 * use of the flag register will impact the result. */
                if i == 0xf && j == 0xf {
                    assert_eq!(c8i.v_regs[0xf], 0);
                } else if j == 0xf {
                    /* Vx - 0x1 = 0xF */
                    assert_eq!(c8i.v_regs[i], 0xf);
                } else if i == 0xf {
                    /* VF (0x1) - Vy = 0xFC */
                    assert_eq!(c8i.v_regs[0xf], 0xfc);
                } else if i == j {
                    assert_eq!(c8i.v_regs[i], 0x00);
                    assert_eq!(c8i.v_regs[0xf], 0);
                } else {
                    assert_eq!(c8i.v_regs[i], 0x0b);
                    assert_eq!(c8i.v_regs[0xf], 1);
                }
            }
        }
    }

    #[test]
    /* Stores the least significant bit of VX in VF and then
     * shifts VX to the right by 1. */
    fn opc_8x06_no_low_bit() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            interpret_instruction(&mut c8i, build_xnn_opc(6, i as u8, 0x10));
            interpret_instruction(&mut c8i, build_xnn_opc(8, i as u8, 0x06));

            if i == 0xf {
                assert_eq!(c8i.v_regs[i], 0);
            } else {
                assert_eq!(c8i.v_regs[0xf], 0);
                assert_eq!(c8i.v_regs[i], 0x08);
            }
        }
    }

    #[test]
    /* Stores the least significant bit of VX in VF and then
     * shifts VX to the right by 1. */
    fn opc_8x06_low_bit() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            interpret_instruction(&mut c8i, build_xnn_opc(6, i as u8, 0x11));
            interpret_instruction(&mut c8i, build_xnn_opc(8, i as u8, 0x06));

            if i == 0xf {
                assert_eq!(c8i.v_regs[i], 0);
            } else {
                assert_eq!(c8i.v_regs[0xf], 1);
                assert_eq!(c8i.v_regs[i], 0x08);
            }
        }
    }

    #[test]
    /* Subtract VY from VX. VF is set to 1 when Vx > Vy, and
     * 0 otherwise. */
    fn opc_8xy7() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            for j in 0..Chip8Instance::NUM_V_REGISTERS {
                let op = build_xyn_opc(8, i as u8, j as u8, 7);

                /* Set destination with a known value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, i as u8, 0x10));
                /* Set source to test value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, j as u8, 5));
                /* Set Vx = Vx & Vy */
                interpret_instruction(&mut c8i, op);

                /* Different from the Add operation, the borrow flag is set
                 * first and then subtraction is performed. This means that
                 * use of the flag register will impact the result. */
                if i == 0xF && j == 0xF {
                    assert_eq!(c8i.v_regs[0xF], 0);
                } else if j == 0xf {
                    /* Vy - 0x1 = 0xF */
                    assert_eq!(c8i.v_regs[i], 0xf0);
                } else if i == 0xf {
                    /* VF (0x1) - Vx = 0xFC */
                    assert_eq!(c8i.v_regs[i], 0x5);
                } else if i == j {
                    assert_eq!(c8i.v_regs[i], 0x00);
                    assert_eq!(c8i.v_regs[0xf], 0);
                } else {
                    assert_eq!(c8i.v_regs[i], 0x0f5);
                    assert_eq!(c8i.v_regs[0xf], 0);
                }
            }
        }
    }

    #[test]
    /* Stores the most significant bit of VX in VF and then
     * shifts VX to the left by 1. */
    fn opc_8x0e_no_high_bit() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            interpret_instruction(&mut c8i, build_xnn_opc(6, i as u8, 0x10));
            interpret_instruction(&mut c8i, build_xnn_opc(8, i as u8, 0x0e));

            if i == 0xf {
                assert_eq!(c8i.v_regs[i], 0);
            } else {
                assert_eq!(c8i.v_regs[0xf], 0);
                assert_eq!(c8i.v_regs[i], 0x20);
            }
        }
    }

    #[test]
    /* Stores the most significant bit of VX in VF and then
     * shifts VX to the left by 1. */
    fn opc_8x0e_high_bit() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            interpret_instruction(&mut c8i, build_xnn_opc(6, i as u8, 0x82));
            interpret_instruction(&mut c8i, build_xnn_opc(8, i as u8, 0x0e));

            if i == 0xf {
                assert_eq!(c8i.v_regs[i], 2);
            } else {
                assert_eq!(c8i.v_regs[0xf], 1);
                assert_eq!(c8i.v_regs[i], 0x4);
            }
        }
    }

    #[test]
    /* Skip next instruction of Vx != Vy. */
    fn opc_9xy0() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            for j in 0..Chip8Instance::NUM_V_REGISTERS {
                let op = build_xyn_opc(9, i as u8, j as u8, 0);

                /* Set destination with a known value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, i as u8, 0x10));
                /* Set source to test value */
                interpret_instruction(&mut c8i, build_xnn_opc(6, j as u8, 5));
                /* Set Vx = Vx & Vy */
                interpret_instruction(&mut c8i, op);

                if i == j {
                    assert_eq!(c8i.pc, Chip8Instance::PROGRAM_LOAD_ADDR);
                } else {
                    assert_eq!(c8i.pc, Chip8Instance::PROGRAM_LOAD_ADDR + 2);
                }

                /* Reset program counter back to the start */
                interpret_instruction(&mut c8i, build_nnn_opc(1, Chip8Instance::PROGRAM_LOAD_ADDR));
            }
        }
    }

    #[test]
    /* LD I, NNN */
    fn opc_annn() {
        let mut c8i = Chip8Instance::default();

        for i in 0xa000..0xb000 {
            interpret_instruction(&mut c8i, i);
            assert_eq!(c8i.i_reg, i & 0xfff);
        }
    }

    #[test]
    /* JP V0, NNN
     * Jump to NNN + V0 */
    fn opc_bnnn_v0_nop() {
        let mut c8i = Chip8Instance::default();

        for i in 0xb000..0xc000 {
            interpret_instruction(&mut c8i, build_xnn_opc(6, 0, 0));
            interpret_instruction(&mut c8i, i);
            assert_eq!(c8i.i_reg, i & 0xfff);
        }
    }

    #[test]
    /* JP V0, NNN
     * Jump to NNN + V0 */
    fn opc_bnnn_v0_overflow() {
        let mut c8i = Chip8Instance::default();

        interpret_instruction(&mut c8i, build_xnn_opc(6, 0, 0xff));
        interpret_instruction(&mut c8i, build_xnn_opc(0xb, 0xf, 0xff));
        assert_eq!(c8i.i_reg, 0x10fe);
    }

    #[test]
    /* RND Vx, NN
     * Set Vx = random byte AND NN.
     */
    fn opc_cxnn() {
        let mut c8i = Chip8Instance::default();
        c8i.rng = Box::new(StepRng::new(4, 0));

        interpret_instruction(&mut c8i, build_xnn_opc(0xc, 0x0, 0xff));
        assert_eq!(c8i.v_regs[0], 4);

        interpret_instruction(&mut c8i, build_xnn_opc(0xc, 0, 0));
        assert_eq!(c8i.v_regs[0], 0);
    }

    #[test]
    fn opc_dxyn_nop() {
        let mut c8i = Chip8Instance::default();

        interpret_instruction(&mut c8i, build_xnn_opc(0x6, 0, 0));
        interpret_instruction(&mut c8i, build_nnn_opc(0xa, 0x300));
        interpret_instruction(&mut c8i, build_xyn_opc(0xd, 0, 0, 1));

        assert_eq!(c8i.v_regs[0xf], 0);
        assert_eq!(c8i.vram[0][0], 0);
    }

    #[test]
    fn opc_dxyn_pixel_cleared() {
        let mut c8i = Chip8Instance::default();

        interpret_instruction(&mut c8i, build_xnn_opc(0x6, 1, 0));
        interpret_instruction(&mut c8i, build_nnn_opc(0xa, 0x300));

        c8i.ram[0x300] = 0x8a;
        /* Write some bits to be cleared in VRAM */
        c8i.vram[0][0] = 0xff;
        c8i.vram[0][4] = 0xff;

        interpret_instruction(&mut c8i, build_xyn_opc(0xd, 1, 1, 1));
        assert_eq!(c8i.v_regs[0xf], 1);
        assert_eq!(c8i.vram[0][0], 0);
    }

    #[test]
    fn opc_dxyn_multiple_bytes() {
        let mut c8i = Chip8Instance::default();

        interpret_instruction(&mut c8i, build_xnn_opc(0x6, 2, 0));
        interpret_instruction(&mut c8i, build_nnn_opc(0xa, 0x300));

        for i in 0x300..0x310 {
            c8i.ram[i] = 0x8a;
        }

        interpret_instruction(&mut c8i, build_xyn_opc(0xd, 2, 2, 0xf));

        /* Check some pixels */
        assert_eq!(c8i.vram[0][0], 0xff);
        assert_eq!(c8i.vram[0][1], 0x00);
        assert_eq!(c8i.vram[0][4], 0xff);

        /* Nothing in VRAM at the start of the test so no pixels are cleared */
        assert_eq!(c8i.v_regs[0xf], 0);
    }

    #[test]
    fn opc_dxyn_wraparound() {
        let mut c8i = Chip8Instance::default();

        interpret_instruction(&mut c8i, build_xnn_opc(0x6, 3, 0));
        interpret_instruction(&mut c8i, build_xnn_opc(0x6, 4, 30));
        interpret_instruction(&mut c8i, build_nnn_opc(0xa, 0x300));

        for i in 0x300..0x310 {
            c8i.ram[i] = 0x8a;
        }

        interpret_instruction(&mut c8i, build_xyn_opc(0xd, 3, 4, 0xf));

        /* Check some pixels */
        assert_eq!(c8i.vram[0][0], 0xff);
        assert_eq!(c8i.vram[0][1], 0x00);
        assert_eq!(c8i.vram[30][0], 0xff);
        assert_eq!(c8i.vram[30][1], 0x0);

        /* Nothing in VRAM at the start of the test so no pixels are cleared */
        assert_eq!(c8i.v_regs[0xf], 0);
    }

    #[test]
    fn opc_ex9e_pressed() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            c8i.keys_pressed[Chip8Key::Key0 as usize] = true;
            interpret_instruction(&mut c8i, build_xnn_opc(0x6, i as u8, 0));
            interpret_instruction(&mut c8i, build_xnn_opc(0xe, i as u8, 0x9e));

            assert_eq!(c8i.pc, Chip8Instance::PROGRAM_LOAD_ADDR + 2);
            interpret_instruction(&mut c8i, build_nnn_opc(1, Chip8Instance::PROGRAM_LOAD_ADDR));
        }
    }

    #[test]
    fn opc_ex9e_not_pressed() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            c8i.keys_pressed[Chip8Key::Key0 as usize] = false;
            interpret_instruction(&mut c8i, build_xnn_opc(0x6, i as u8, 0));
            interpret_instruction(&mut c8i, build_xnn_opc(0xe, i as u8, 0x9e));

            assert_eq!(c8i.pc, Chip8Instance::PROGRAM_LOAD_ADDR);
            interpret_instruction(&mut c8i, build_nnn_opc(1, Chip8Instance::PROGRAM_LOAD_ADDR));
        }
    }

    #[test]
    fn opc_exa1_pressed() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            c8i.keys_pressed[Chip8Key::Key0 as usize] = true;
            interpret_instruction(&mut c8i, build_xnn_opc(0x6, i as u8, 0));
            interpret_instruction(&mut c8i, build_xnn_opc(0xe, i as u8, 0xa1));

            assert_eq!(c8i.pc, Chip8Instance::PROGRAM_LOAD_ADDR);
            interpret_instruction(&mut c8i, build_nnn_opc(1, Chip8Instance::PROGRAM_LOAD_ADDR));
        }
    }

    #[test]
    fn opc_exa1_not_pressed() {
        let mut c8i = Chip8Instance::default();

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            c8i.keys_pressed[Chip8Key::Key0 as usize] = false;
            interpret_instruction(&mut c8i, build_xnn_opc(0x6, i as u8, 0));
            interpret_instruction(&mut c8i, build_xnn_opc(0xe, i as u8, 0xa1));

            assert_eq!(c8i.pc, Chip8Instance::PROGRAM_LOAD_ADDR + 2);
            interpret_instruction(&mut c8i, build_nnn_opc(1, Chip8Instance::PROGRAM_LOAD_ADDR));
        }
    }

    #[test]
    fn opc_fx07() {
        let mut c8i = Chip8Instance::default();
        c8i.delay_timer = 42;

        for i in 0..Chip8Instance::NUM_V_REGISTERS {
            interpret_instruction(&mut c8i, build_xnn_opc(0xf, i as u8, 7));
            assert_eq!(c8i.v_regs[i], 42);
        }
    }
}
