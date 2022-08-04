//! Module to handle the html5 canvas interactions
use crate::machine::MachineState;
use bitvec::prelude::*;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::{Clamped, JsCast};
#[cfg(target_arch = "wasm32")]
use web_sys::{CanvasRenderingContext2d, HtmlCanvasElement, ImageData};

const INITIAL_VIDEO_MEM_IDX: usize = 0x2400 - 0x2000;
const END_VIDEO_RAM: usize = INITIAL_VIDEO_MEM_IDX + (0x4000 - 0x2400);
const DISPLAY_WIDTH: usize = 224;
const DISPLAY_HEIGHT: usize = 256;

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

    let mut vid_bits: [u8; 4 * DISPLAY_HEIGHT * DISPLAY_WIDTH] =
        [0; 4 * DISPLAY_HEIGHT * DISPLAY_WIDTH];
    let mut mem_x = 0;
    let mut mem_y = 0;
    for bit in BitVec::<Lsb0, u8>::from_slice(
        &machine.mem_map.rw_mem[INITIAL_VIDEO_MEM_IDX..END_VIDEO_RAM],
    )
    .unwrap()
    {
        let im_x = mem_y;
        let im_y = (DISPLAY_HEIGHT - 1) - mem_x;
        if bit {
            if ((im_y > 180) && (im_y < 240)) || ((im_y >= 240) && (im_x > 20 && im_x < 60)) {
                // Green
                vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 0] = 0x62;
                vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 1] = 0xde;
                vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 2] = 0x6d;
                vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 3] = 0xff;
            } else if im_y < 50 && im_y > 33 {
                // Red
                vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 0] = 0xf8;
                vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 1] = 0x3b;
                vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 2] = 0x3a;
                vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 3] = 0xff;
            } else {
                vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 0] = 0xff;
                vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 1] = 0xff;
                vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 2] = 0xff;
                vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 3] = 0xff;
            }
        } else {
            vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 0] = 0x00;
            vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 1] = 0x00;
            vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 2] = 0x00;
            vid_bits[4 * (im_y * DISPLAY_WIDTH + im_x) + 3] = 0xff; // Only set alpha
        }
        if mem_x == (DISPLAY_HEIGHT - 1) {
            mem_y += 1;
        }
        mem_x = (mem_x + 1) % DISPLAY_HEIGHT;
    }

    let clamped_vid_bits = Clamped(vid_bits.as_slice());
    let imdata = ImageData::new_with_u8_clamped_array_and_sh(
        clamped_vid_bits,
        DISPLAY_WIDTH as u32,
        DISPLAY_HEIGHT as u32,
    )
    .unwrap();
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
