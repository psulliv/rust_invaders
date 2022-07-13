mod debug_utils;
mod display_output;
mod eighty_eighty_emulator;
mod machine;
mod space_invaders_rom;
use eighty_eighty_emulator::{MemMap, ProcessorState};
use machine::{start_keyboard_listeners, MachineState};
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn js_entry_point() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();
    let machine = machine::MachineState::new();
    start_keyboard_listeners(&machine);
    emulation_loop(machine);
    Ok(())
}

pub fn emulation_loop(mut this_machine: MachineState) -> String {
    loop {
        crate::display_output::write_canvas_element(&this_machine);
        this_machine.iterate_processor_state();
    }
}
