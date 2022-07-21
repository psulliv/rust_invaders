//! Module to handle the html5 canvas interactions
use crate::machine::MachineState;
use crate::space_invaders_rom::SPACE_INVADERS_ROM;

#[cfg(target_arch = "wasm32")]
pub fn write_canvas_element(machine: &MachineState) {
    use wasm_bindgen::JsCast;
    use web_sys::{CanvasRenderingContext2d, HtmlCanvasElement};
    let document = web_sys::window().unwrap().document().unwrap();

    // Make a canvas element on the body
    let body = document.body().unwrap();
    // let canvas_node = document.create_element("canvas").unwrap();
    // body.append_child(&canvas_node);
    let canvas = document.get_element_by_id("canvas").unwrap();
    let canvas: web_sys::HtmlCanvasElement = canvas
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .map_err(|_| ())
        .unwrap();

    let context = canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .unwrap();

    // kind of dirty way to access the video memory, there should
    // be an abstraction for this.
    // The machine struct should be a decent place to store a method that gets
    // the video memory. Since we are just reading it maybe we don't need a lock?
    // let video_ram = &machine.mem_map.rw_mem
    //     [(SPACE_INVADERS_ROM.len() - 0x2400)..(SPACE_INVADERS_ROM.len() - 0x3FFF)];
    // for (idx, &some_byte) in video_ram.iter().enumerate() {

    // Split the byte into bits for writing
    // }
}
#[cfg(target_arch = "x86_64")]
pub fn write_console_term(machine: &MachineState) {
    let initial_video_mem_idx = 0x2400 - SPACE_INVADERS_ROM.len() - 1;
    let end_video_ram = initial_video_mem_idx + 7167;
    let mut counter = 0;
    for &some_byte in machine.mem_map.rw_mem[initial_video_mem_idx..end_video_ram].iter() {
        print!("{:#010b} ", some_byte);
        if counter == 63 {
            println!();
            counter = 0;
        }
        counter += 1;
    }
}
