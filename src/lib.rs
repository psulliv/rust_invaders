//!Space Invaders, (C) Taito 1978, Midway 1979
//!
//!CPU: Intel 8080 @ 2MHz (CPU similar to the (newer) Zilog Z80)
//!
//!Interrupts: $cf (RST 8) at the start of vblank, $d7 (RST $10) at the end of vblank.
//!
//!Video: 256(x)*224(y) @ 60Hz, vertical monitor. Colours are simulated with a
//!plastic transparent overlay and a background picture.
//!Video hardware is very simple: 7168 bytes 1bpp bitmap (32 bytes per scanline).
//!
//!Sound: SN76477 and samples.
//!
//!
//!Ports:
//!	Read 1
//!	BIT	0	coin (0 when active)
//!		1	P2 start button
//!		2	P1 start button
//!		3	?
//!		4	P1 shoot button
//!		5	P1 joystick left
//!		6	P1 joystick right
//!		7	?
//!
//!	Read 2
//!	BIT	0,1	dipswitch number of lives (0:3,1:4,2:5,3:6)
//!		2	tilt 'button'
//!		3	dipswitch bonus life at 1:1000,0:1500
//!		4	P2 shoot button
//!		5	P2 joystick left
//!		6	P2 joystick right
//!		7	dipswitch coin info 1:off,0:on
//!
//!	Read 3		shift register result
//!
//!	Write 2		shift register result offset (bits 0,1,2)
//!	Write 3		sound related
//!	Write 4		fill shift register
//!	Write 5		sound related
//!	Write 6		strange 'debug' port? eg. it writes to this port when
//!			it writes text to the screen (0=a,1=b,2=c, etc)
//!
//!	(write ports 3,5,6 can be left unemulated, read port 1=$01 and 2=$00
//!	will make the game run, but but only in attract mode)
//!
//!I haven't looked into sound details.
//!
//!16 bit shift register:
//!	f              0	bit
//!	xxxxxxxxyyyyyyyy
//!
//!	Writing to port 4 shifts x into y, and the new value into x, eg.
//!	$0000,
//!	write $aa -> $aa00,
//!	write $ff -> $ffaa,
//!	write $12 -> $12ff, ..
//!
//!	Writing to port 2 (bits 0,1,2) sets the offset for the 8 bit result, eg.
//!	offset 0:
//!	rrrrrrrr		result=xxxxxxxx
//!	xxxxxxxxyyyyyyyy
//!
//!	offset 2:
//!	  rrrrrrrr	result=xxxxxxyy
//!	xxxxxxxxyyyyyyyy
//!
//!	offset 7:
//!	       rrrrrrrr	result=xyyyyyyy
//!	xxxxxxxxyyyyyyyy
//!
//!	Reading from port 3 returns said result.
//!
//!Overlay dimensions (screen rotated 90 degrees anti-clockwise):
//!	,_______________________________.
//!	|WHITE            ^             |
//!	|                32             |
//!	|                 v             |
//!	|-------------------------------|
//!	|RED              ^             |
//!	|                32             |
//!	|                 v             |
//!	|-------------------------------|
//!	|WHITE                          |
//!	|         < 224 >               |
//!	|                               |
//!	|                 ^             |
//!	|                120            |
//!	|                 v             |
//!	|                               |
//!	|                               |
//!	|                               |
//!	|-------------------------------|
//!	|GREEN                          |
//!	| ^                  ^          |
//!	|56        ^        56          |
//!	| v       72         v          |
//!	|____      v      ______________|
//!	|  ^  |          | ^            |
//!	|<16> |  < 118 > |16   < 122 >  |
//!	|  v  |          | v            |
//!	|WHITE|          |         WHITE|
//!	`-------------------------------'
//!
//!	Way of out of proportion :P

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;


#[wasm_bindgen]
extern {
    fn alert(s: &str);
}

#[wasm_bindgen]
struct KeyboardState {
    coin: bool,
    start: bool,
    shoot: bool,
    left: bool,
    right: bool,
}

#[wasm_bindgen]
pub fn keydown_handler(event: web_sys::KeyboardEvent) {
    match event.key().as_str() {
        "ArrowLeft" => alert("ArrowLeft down"),
        "ArrowRight" => alert("ArrowRight down"),
        "C" => alert("C down"),
        "S" => alert("S down"),
        " " => alert("Space down"),
        other => alert(&event.key()),
    }
}

#[wasm_bindgen]
pub fn keyup_handler(event: web_sys::KeyboardEvent) {
    let window = web_sys::window().expect("no global `window` exists");
    let document = window.document().expect("should have a document on window");
    let canvas = document.get_element_by_id("display").unwrap().dyn_into::<web_sys::HtmlCanvasElement>().unwrap();
    let context = canvas.get_context("2d").unwrap().unwrap().dyn_into::<web_sys::CanvasRenderingContext2d>().unwrap();
    context.fill_text(&event.key(), 10.0, 50.0).expect("unable to draw to canvas");
    //alert(&event.key());
}

#[wasm_bindgen]
pub fn start_keyboard_listeners() {
    let window = web_sys::window().expect("no global `window` exists");

    let keydown_closure = Closure::wrap(Box::new(keydown_handler) as Box<dyn FnMut(_)>);
    window.add_event_listener_with_callback("keydown", keydown_closure.as_ref().unchecked_ref()).unwrap();
    keydown_closure.forget();

    let keyup_closure = Closure::wrap(Box::new(keyup_handler) as Box<dyn FnMut(_)>);
    window.add_event_listener_with_callback("keyup", keyup_closure.as_ref().unchecked_ref()).unwrap();
    keyup_closure.forget();
}

/*
mod debug_utils;
mod eighty_eighty_emulator;
mod space_invaders_rom;

use eighty_eighty_emulator::ProcessorState;

fn emulation_loop(mut this_processor: ProcessorState, invaders_rom: &[u8; 8192]) -> ! {
    loop {
        eighty_eighty_emulator::iterate_processor_state(&mut this_processor, &invaders_rom);
    }
}

fn main() {
    let this_processor = ProcessorState::new();
    emulation_loop(this_processor, &space_invaders_rom::SPACE_INVADERS_ROM);
}
*/