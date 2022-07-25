#![allow(unused)]
pub mod debug_utils;
mod display_output;
mod eighty_eighty_emulator;
pub mod machine;
mod space_invaders_rom;
use eighty_eighty_emulator::{MemMap, ProcessorState};
use fluvio_wasm_timer::Delay;
use machine::{start_keyboard_listeners, MachineState};
use std::io;
use std::io::prelude::*;
use std::time::Duration;
use std::{thread, time};

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen_futures::spawn_local;

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen(start)]
pub async fn js_entry_point() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    let machine = machine::MachineState::new();
    start_keyboard_listeners(&machine);
    emulation_loop(machine).await;
    unsafe {
        web_sys::console::log_1(&"Shouldn't print".into());
    }
    Ok(())
}

pub async fn emulation_loop(mut this_machine: MachineState) {
    let mut count = 0;
    loop {
        #[cfg(target_arch = "wasm32")]
        {
            debug_utils::debug_console_print(&debug_utils::opcode_printer(&this_machine));
            for _ in 0..100000 {
                this_machine.iterate_processor_state();
            }

            debug_utils::debug_console_print(&debug_utils::processor_state_printer(&this_machine));
            Delay::new(Duration::new(0, 100_000_000)).await;
            crate::display_output::write_canvas_element(&this_machine);
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
