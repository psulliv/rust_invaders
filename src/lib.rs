pub mod debug_utils;
mod display_output;
mod eighty_eighty_emulator;
pub mod machine;
pub mod space_invaders_rom;
use eighty_eighty_emulator::{MemMap, ProcessorState};
#[cfg(target_arch = "wasm32")]
use fluvio_wasm_timer::Delay;
#[cfg(target_arch = "wasm32")]
use machine::start_keyboard_listeners;
use machine::MachineState;
use std::time::Duration;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen(start)]
pub async fn js_entry_point() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    let machine = MachineState::new();
    start_keyboard_listeners(&machine);
    wasm_bindgen_futures::spawn_local(emulation_loop(machine));
    Ok(())
}

#[cfg(target_arch = "wasm32")]
pub async fn emulation_loop(mut this_machine: MachineState) {
    let mut count_ints = 0;
    loop {
        {
            this_machine.iterate_processor_state();
            if this_machine.interrupt_due() {
                if count_ints > 20 {
                    count_ints = 0;
                    crate::display_output::write_canvas_element(&this_machine);
                    Delay::new(Duration::new(0, 50_000)).await.unwrap();
                }
                this_machine.do_next_interrupt();
                count_ints += 1;
            }
        }
    }
}
