#![allow(unused)]

//!```text
//!Interrupts: $cf (RST 8) at the start of vblank, $d7 (RST $10) at the end of vblank.
//!
//!Video: 256(x)*224(y) @ 60Hz, vertical monitor. Colours are simulated with a
//!plastic transparent overlay and a background picture.
//!Video hardware is very simple: 7168 bytes 1bpp bitmap (32 bytes per scanline).
//!
//!Sound: SN76477 and samples.
//!
//!
//!
//!I haven't looked into sound details.
//!
//!16 bit shift register:
//!     f              0        bit
//!     xxxxxxxxyyyyyyyy
//!
//!     Writing to port 4 shifts x into y, and the new value into x, eg.
//!     $0000,
//!     write $aa -> $aa00,
//!     write $ff -> $ffaa,
//!     write $12 -> $12ff, ..
//!
//!     Writing to port 2 (bits 0,1,2) sets the offset for the 8 bit result, eg.
//!     offset 0:
//!     rrrrrrrr                result=xxxxxxxx
//!     xxxxxxxxyyyyyyyy
//!
//!     offset 2:
//!       rrrrrrrr      result=xxxxxxyy
//!     xxxxxxxxyyyyyyyy
//!
//!     offset 7:
//!            rrrrrrrr result=xyyyyyyy
//!     xxxxxxxxyyyyyyyy
//!
//!     Reading from port 3 returns said result.
//!
//!Overlay dimensions (screen rotated 90 degrees anti-clockwise):
//!     ,_______________________________.
//!     |WHITE            ^             |
//!     |                32             |
//!     |                 v             |
//!     |-------------------------------|
//!     |RED              ^             |
//!     |                32             |
//!     |                 v             |
//!     |-------------------------------|
//!     |WHITE                          |
//!     |         < 224 >               |
//!     |                               |
//!     |                 ^             |
//!     |                120            |
//!     |                 v             |
//!     |                               |
//!     |                               |
//!     |                               |
//!     |-------------------------------|
//!     |GREEN                          |
//!     | ^                  ^          |
//!     |56        ^        56          |
//!     | v       72         v          |
//!     |____      v      ______________|
//!     |  ^  |          | ^            |
//!     |<16> |  < 118 > |16   < 122 >  |
//!     |  v  |          | v            |
//!     |WHITE|          |         WHITE|
//!     `-------------------------------'
//!
//!     Way of out of proportion :P
//!```

use crate::{MemMap, ProcessorState};
use std::sync::{Arc, Mutex};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::console;

#[derive(Clone, Copy)]
pub enum Button {
    Fire,
    Left,
    Right,
    P1Start,
    P2Start,
    P1Shoot,
    P2Shoot,
    P1Left,
    P2Left,
    P1Right,
    P2Right,
    Coin,
    NumLivesSwitch0,
    NumLivesSwitch1,
    Tilt,
    BonusLife,
    CoinInfo,
}

#[derive(Debug)]
pub struct PortState {
    ///```text
    ///Ports:
    ///     Read 1
    ///     BIT     0       coin (0 when active)
    ///             1       P2 start button
    ///             2       P1 start button
    ///             3       ?
    ///             4       P1 shoot button
    ///             5       P1 joystick left
    ///             6       P1 joystick right
    ///             7       ?
    ///
    ///     Read 2
    ///     BIT     0,1     dipswitch number of lives (0:3,1:4,2:5,3:6)
    ///             2       tilt 'button'
    ///             3       dipswitch bonus life at 1:1000,0:1500
    ///             4       P2 shoot button
    ///             5       P2 joystick left
    ///             6       P2 joystick right
    ///             7       dipswitch coin info 1:off,0:on
    ///
    ///     Read 3          shift register result
    ///
    ///     Write 2         shift register result offset (bits 0,1,2)
    ///     Write 3         sound related
    ///     Write 4         fill shift register
    ///     Write 5         sound related
    ///     Write 6         strange 'debug' port? eg. it writes to this port when
    ///                     it writes text to the screen (0=a,1=b,2=c, etc)
    ///
    ///     (write ports 3,5,6 can be left unemulated, read port 1=$01 and 2=$00
    ///     will make the game run, but but only in attract mode)
    ///```
    pub read_port_0: u8,
    pub read_port_1: u8,
    pub read_port_2: u8,
    pub read_port_3: u8,
    pub write_port_1: u8,
    pub write_port_2: u8,
    pub write_port_4: u8,
    pub shift_register: u16,
}

impl PortState {
    pub fn button_down(&mut self, button: Button) {
        match button {
            Button::Left => {
                self.read_port_0 |= 0b1 << 5;
            }
            Button::Right => {
                self.read_port_0 |= 0b1 << 6;
            }
            Button::Fire => {
                self.read_port_0 |= 0b1 << 4;
            }
            Button::P1Start => {
                self.read_port_1 |= 0b1 << 2;
            }
            Button::P2Start => {
                self.read_port_1 |= 0b1 << 1;
            }
            Button::P1Shoot => {
                self.read_port_1 |= 0b1 << 4;
            }
            Button::P2Shoot => {
                self.read_port_2 |= 0b1 << 4;
            }
            Button::P1Left => {
                self.read_port_1 |= 0b1 << 5;
            }
            Button::P2Left => {
                self.read_port_2 |= 0b1 << 5;
            }
            Button::P1Right => {
                self.read_port_1 |= 0b1 << 6;
            }
            Button::P2Right => {
                self.read_port_2 |= 0b1 << 6;
            }
            Button::Coin => {
                self.read_port_1 |= 0b1;
            }
            Button::NumLivesSwitch0 => {
                self.read_port_2 &= 0b1;
            }
            Button::NumLivesSwitch1 => {
                self.read_port_2 &= 0b1 << 1;
            }
            Button::Tilt => {
                self.read_port_2 |= 0b1 << 2;
            }
            Button::BonusLife => {
                self.read_port_2 |= 0b1 << 3;
            }
            Button::CoinInfo => {
                self.read_port_2 &= !(0b1 << 7);
            }
        }
    }
    pub fn button_up(&mut self, button: Button) {
        match button {
            Button::Left => {
                self.read_port_0 &= !(0b1 << 5);
            }
            Button::Right => {
                self.read_port_0 &= !(0b1 << 6);
            }
            Button::Fire => {
                self.read_port_0 &= !(0b1 << 4);
            }
            Button::P1Start => {
                self.read_port_1 &= !(0b1 << 2);
            }
            Button::P2Start => {
                self.read_port_1 &= !(0b1 << 1);
            }
            Button::P1Shoot => {
                self.read_port_1 &= !(0b1 << 4);
            }
            Button::P2Shoot => {
                self.read_port_2 &= !(0b1 << 4);
            }
            Button::P1Left => {
                self.read_port_1 &= !(0b1 << 5);
            }
            Button::P2Left => {
                self.read_port_2 &= !(0b1 << 5);
            }
            Button::P1Right => {
                self.read_port_1 &= !(0b1 << 6);
            }
            Button::P2Right => {
                self.read_port_2 &= !(0b1 << 6);
            }
            Button::Coin => {
                // leave the coin
                // self.read_port_1 |= 0b1;
            }
            Button::NumLivesSwitch0 => {
                self.read_port_2 &= !(0b1);
            }
            Button::NumLivesSwitch1 => {
                self.read_port_2 &= !(0b1 << 1);
            }
            Button::Tilt => {
                self.read_port_2 &= !(0b1 << 2);
            }
            Button::BonusLife => {
                self.read_port_2 &= !(0b1 << 3);
            }
            Button::CoinInfo => {
                self.read_port_2 |= 0b1 << 7;
            }
        }
    }
}

pub struct MachineState {
    pub port_state: Arc<Mutex<PortState>>,
    pub processor_state: ProcessorState,
    pub mem_map: MemMap,
}

impl MachineState {
    pub fn new() -> Self {
        MachineState {
            port_state: Arc::new(Mutex::new(PortState {
                read_port_0: 0b0000_1110,
                read_port_1: 0b0000_0000,
                read_port_2: 0b1000_0000,
                read_port_3: 0b0000_0000,
                write_port_1: 0b0000_0000,
                write_port_2: 0b0000_0000,
                write_port_4: 0b0000_0000,
                shift_register: 0b0000_0000,
            })),
            processor_state: ProcessorState::new(),
            mem_map: MemMap::new(),
        }
    }
}

impl Default for MachineState {
    fn default() -> Self {
        Self::new()
    }
}

fn map_keyboard_to_button(key: &str) -> Option<Button> {
    match key {
        "KeyZ" => Some(Button::Left),
        "KeyX" => Some(Button::Right),
        "KeyA" => Some(Button::P1Left),
        "KeyD" => Some(Button::P1Right),
        "ArrowLeft" => Some(Button::P2Left),
        "ArrowRight" => Some(Button::P2Right),
        "Enter" => Some(Button::P1Shoot),
        "Space" => Some(Button::P2Shoot),
        "Digit1" => Some(Button::P1Start),
        "Digit2" => Some(Button::P2Start),
        "KeyC" => Some(Button::Coin),
        _ => None,
    }
}

pub fn start_keyboard_listeners(m: &MachineState) {
    let window = web_sys::window().expect("no global `window` exists");
    let down_m = m.port_state.clone();
    let keydown_closure = Closure::wrap(Box::new(move |event: web_sys::KeyboardEvent| {
        if let Some(button) = map_keyboard_to_button(&(event.code())) {
            let mut l_portstate = down_m.lock().unwrap();
            l_portstate.button_down(button);
            unsafe { console::log_1(&format!("Keydown: state is {:#?}", l_portstate).into()) };
        }
    }) as Box<dyn FnMut(_)>);
    window
        .add_event_listener_with_callback("keydown", keydown_closure.as_ref().unchecked_ref())
        .unwrap();
    keydown_closure.forget();

    let up_m = m.port_state.clone();
    let keyup_closure = Closure::wrap(Box::new(move |event: web_sys::KeyboardEvent| {
        if let Some(button) = map_keyboard_to_button(&(event.code())) {
            let mut l_portstate = up_m.lock().unwrap();
            l_portstate.button_up(button);
            unsafe { console::log_1(&format!("Keyup: state is {:#?}", l_portstate).into()) };
        }
    }) as Box<dyn FnMut(_)>);
    window
        .add_event_listener_with_callback("keyup", keyup_closure.as_ref().unchecked_ref())
        .unwrap();
    keyup_closure.forget();
}
