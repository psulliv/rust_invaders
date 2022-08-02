#[tokio::main]
async fn main() {
    #[cfg(target_arch = "x86_64")]
    {
        use invaders::debug_utils;
        let mut some_memory: Vec<u8> = vec![0; 0x4000];
        some_memory[..invaders::space_invaders_rom::SPACE_INVADERS_ROM.len()]
            .clone_from_slice(&invaders::space_invaders_rom::SPACE_INVADERS_ROM);
        let some_memory: *mut u8 = some_memory.as_mut_ptr();
        let mut ref_machine = debug_utils::RefState::new(some_memory);
        let mut machine = invaders::machine::MachineState::new();
        let mut last_interrupt_one = true;
        loop {
            if machine.interrupt_due() {
                if last_interrupt_one {
                    if ref_machine.ints_enabled() {
                        last_interrupt_one = false;
                        unsafe { debug_utils::GenerateInterrupt(&mut ref_machine, 2) };
                    }
                } else {
                    if ref_machine.ints_enabled() {
                        last_interrupt_one = true;
                        unsafe { debug_utils::GenerateInterrupt(&mut ref_machine, 1) };
                    }
                }

                // this can be out of sync, check the init fn to see whats first
                machine.do_next_interrupt();
            }

            unsafe { debug_utils::Emulate8080Op(&mut ref_machine) };
            machine.iterate_processor_state();

            if !ref_machine.compare_to_r(&machine.processor_state) {
                println!(
                    "Different states! {:#?} \n {:#?}",
                    &ref_machine, &machine.processor_state
                );
                break;
            }
        }
    }
}
