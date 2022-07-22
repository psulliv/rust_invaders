//! Module to handle the html5 canvas interactions
use crate::machine::MachineState;
use crate::space_invaders_rom::SPACE_INVADERS_ROM;
use bitvec::prelude::*;
use wasm_bindgen::Clamped;

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
    context.begin_path();
    context
        .arc(75.0, 75.0, 50.0, 0.0, core::f64::consts::PI * 2.0)
        .unwrap();
    let mut image = context
        .create_image_data_with_sw_and_sh(224.0, 256.0)
        .unwrap();

    let mut image_data = image.data();

    // ROM up until 0x2000
    let initial_video_mem_idx = 0x2400 - 0x2000;
    // 7k video ram 0x2400 - 0x3FFF
    let end_video_ram = initial_video_mem_idx + (0x4000 - 0x2400);
    let vid_bits = BitVec::<Lsb0, u8>::from_slice(
        &machine.mem_map.rw_mem[initial_video_mem_idx..end_video_ram],
    )
    .unwrap();

    for (idx, bval) in vid_bits.iter().enumerate() {
        if *bval {
            image_data[idx * 4 + 0] = 128;
            image_data[idx * 4 + 3] = 128;
            image_data[idx * 4 + 1] = 128;
            image_data[idx * 4 + 2] = 128;
        } else {
            image_data[idx * 4 + 0] = 64;
            image_data[idx * 4 + 3] = 255;
            image_data[idx * 4 + 1] = 64;
            image_data[idx * 4 + 2] = 64;
        }
    }
    context.put_image_data(&image, 0.0, 0.0);
    context.draw_image_with_html_canvas_element(&canvas, 0.0, 0.0);
    context.stroke();
}
#[cfg(target_arch = "x86_64")]
pub fn write_console_term(machine: &MachineState) {
    let initial_video_mem_idx = 0x2400 - SPACE_INVADERS_ROM.len() - 1;
    let end_video_ram = initial_video_mem_idx + 7167;
    let mut counter = 0;
    for &some_byte in machine.mem_map.rw_mem[initial_video_mem_idx..end_video_ram].iter() {
        print!("{:#010b} ", some_byte);
        if counter == 64 {
            println!();
            counter = 0;
        }
        counter += 1;
    }
}
