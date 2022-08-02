//! Module to handle the html5 canvas interactions
use crate::machine::MachineState;
use bitvec::prelude::*;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::{Clamped, JsCast};
#[cfg(target_arch = "wasm32")]
use web_sys::{CanvasRenderingContext2d, HtmlCanvasElement, ImageData};

const INITIAL_VIDEO_MEM_IDX: usize = 0x2400 - 0x2000;
const END_VIDEO_RAM: usize = INITIAL_VIDEO_MEM_IDX + (0x4000 - 0x2400);

#[cfg(target_arch = "wasm32")]
pub fn write_canvas_element(machine: &MachineState) {
    let document = web_sys::window().unwrap().document().unwrap();

    // Make a canvas element on the body
    // let body = document.body().unwrap();
    // let canvas_node = document.create_element("canvas").unwrap();
    // body.append_child(&canvas_node);
    let canvas = document.get_element_by_id("canvas").unwrap();
    let canvas: HtmlCanvasElement = canvas
        .dyn_into::<HtmlCanvasElement>()
        .map_err(|_| ())
        .unwrap();

    let context = canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into::<CanvasRenderingContext2d>()
        .unwrap();

    let vid_bits: Vec<u8> = BitVec::<Lsb0, u8>::from_slice(
        &machine.mem_map.rw_mem[INITIAL_VIDEO_MEM_IDX..END_VIDEO_RAM],
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
    context.put_image_data(&imdata, 0.0, 0.0).unwrap();

}
#[cfg(target_arch = "x86_64")]
#[allow(unused)]
pub fn write_console_term(machine: &MachineState) {
    // 7k video ram 0x2400 - 0x3FFF

    let vid_bits: Vec<char> = BitVec::<Lsb0, u8>::from_slice(
        &machine.mem_map.rw_mem[INITIAL_VIDEO_MEM_IDX..END_VIDEO_RAM],
    )
    .unwrap()
    .iter()
    .map(|bref| if *bref { '\u{2588}' } else { ' ' })
    .collect();

    let mut count = 0;
    print!("{}[2J", 27 as char);
    for pixel in vid_bits.iter() {
        print!("{}", pixel);
        count += 1;
        if count == 256 {
            println!();
            count = 0;
        }
    }
}
