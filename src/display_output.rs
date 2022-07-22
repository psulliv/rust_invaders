//! Module to handle the html5 canvas interactions
use crate::machine::MachineState;
use crate::space_invaders_rom::SPACE_INVADERS_ROM;
use bitvec::prelude::*;
use std::iter::Chain;
use wasm_bindgen::{prelude::*, Clamped, JsCast};

#[cfg(target_arch = "wasm32")]
pub fn write_canvas_element(machine: &MachineState) {
    use wasm_bindgen::JsCast;
    use web_sys::{CanvasRenderingContext2d, HtmlCanvasElement, ImageData};

    use crate::debug_utils;
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

    // ROM up until 0x2000
    let initial_video_mem_idx = 0x2400 - 0x2000;
    // 7k video ram 0x2400 - 0x3FFF
    let end_video_ram = initial_video_mem_idx + (0x4000 - 0x2400);

    let vid_bits: Vec<u8> = BitVec::<Lsb0, u8>::from_slice(
        &machine.mem_map.rw_mem[initial_video_mem_idx..end_video_ram],
    )
    .unwrap()
    .iter()
    .map(|bref| {
        if *bref {
            [0x00, 0x00, 0x00, 0xff]
        } else {
            [0x00, 0x00, 0x00, 0x00]
        }
    })
    .flatten()
    .collect();

    let clamped_vid_bits = Clamped(vid_bits.as_slice());
    let imdata = ImageData::new_with_u8_clamped_array_and_sh(clamped_vid_bits, 256, 224).unwrap();
    context.put_image_data(&imdata, 0.0, 0.0);
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
