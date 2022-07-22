#![allow(unused)]
pub mod debug_utils;
mod display_output;
mod eighty_eighty_emulator;
pub mod machine;
mod space_invaders_rom;
use eighty_eighty_emulator::{MemMap, ProcessorState};
use machine::{start_keyboard_listeners, MachineState};
use std::io;
use std::io::prelude::*;
use std::{thread, time};

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen(start)]
pub fn js_entry_point() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    let machine = machine::MachineState::new();
    start_keyboard_listeners(&machine);
    emulation_loop(machine);
    Ok(())
}

pub fn emulation_loop(mut this_machine: MachineState) {
    let mut count = 0;
    loop {
        #[cfg(target_arch = "wasm32")]
        {
            // thread::sleep(time::Duration::from_millis(500));
            // Todo: This write should only happen once every 60 millis
            count += 1;
            debug_utils::debug_console_print(&debug_utils::opcode_printer(&this_machine));
            this_machine.iterate_processor_state();
            debug_utils::debug_console_print(&debug_utils::processor_state_printer(&this_machine));
            if count == 100000 {
                crate::display_output::write_canvas_element(&this_machine);
                break;
            }
        }
        #[cfg(target_arch = "x86_64")]
        {
            print!("{} ", count);
            debug_utils::opcode_printer(&this_machine);
            this_machine.iterate_processor_state();
            debug_utils::debug_console_print(debug_utils::processor_state_printer(&this_machine));
            println!();
            count += 1;
            if count % 100 == 0 {}
            if count > 100000 {
                break;
            }
        }
    }
}
