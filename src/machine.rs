// Ports:    
// Read 1    
// BIT 0   coin (0 when active)    
//     1   P2 start button    
//     2   P1 start button    
//     3   ?    
//     4   P1 shoot button    
//     5   P1 joystick left    
//     6   P1 joystick right    
//     7   ?

// Read 2    
// BIT 0,1 dipswitch number of lives (0:3,1:4,2:5,3:6)    
//     2   tilt 'button'    
//     3   dipswitch bonus life at 1:1000,0:1500    
//     4   P2 shoot button    
//     5   P2 joystick left    
//     6   P2 joystick right    
//     7   dipswitch coin info 1:off,0:on    

// Read 3      shift register result    

// Write 2     shift register result offset (bits 0,1,2)    
// Write 3     sound related    
// Write 4     fill shift register    
// Write 5     sound related    
// Write 6     strange 'debug' port? eg. it writes to this port when    
//         it writes text to the screen (0=a,1=b,2=c, etc)    

// (write ports 3,5,6 can be left unemulated, read port 1=$01 and 2=$00    
// will make the game run, but but only in attract mode)  

struct PortState {
    read_port_1: u8,
    read_port_2: u8,
    read_port_3: u8,
    write_port_1: u8,
    write_port_2: u8,
    write_port_4: u8,
}

pub enum Button {
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

pub struct MachineState {
    port_state: PortState,
    // Todo: Move Processor State within Machine State
}

impl MachineState {
    pub fn new() -> Self {
        MachineState {
            port_state: PortState {
                read_port_1: 0b0000_0001,
                read_port_2: 0b0000_0000,
                read_port_3: 0b0000_0000,
                write_port_1: 0b0000_0000,
                write_port_2: 0b0000_0000,
                write_port_4: 0b0000_0000,
            },
        }
    }
    pub fn button_down(mut self, button: Button) {
        match button {
            Button::P1Start => {
                self.port_state.read_port_1 |= 0b1 << 2;
            },
            Button::P2Start => {
                self.port_state.read_port_1 |= 0b1 << 1;
            },
            Button::P1Shoot => {
                self.port_state.read_port_1 |= 0b1 << 4;
            },
            Button::P2Shoot => {
                self.port_state.read_port_2 |= 0b1 << 4;
            },
            Button::P1Left => {
                self.port_state.read_port_1 |= 0b1 << 5;
            },
            Button::P2Left => {
                self.port_state.read_port_2 |= 0b1 << 5;
            },
            Button::P1Right => {
                self.port_state.read_port_1 |= 0b1 << 6;
            },
            Button::P2Right => {
                self.port_state.read_port_2 |= 0b1 << 6;
            },
            Button::Coin => {
                self.port_state.read_port_1 &= !0b1;
            },
            Button::NumLivesSwitch0 => {
                self.port_state.read_port_2 &= 0b1;
            },
            Button::NumLivesSwitch1 => {
                self.port_state.read_port_2 &= 0b1 << 1;
            },
            Button::Tilt => {
                self.port_state.read_port_2 |= 0b1 << 2;
            },
            Button::BonusLife => {
                self.port_state.read_port_2 |= 0b1 << 3;
            },
            Button::CoinInfo => {
                self.port_state.read_port_2 &= !(0b1 << 7);
            },
        }
    }
    pub fn button_up(mut self, button: Button) {
        match button {
            Button::P1Start => {
                self.port_state.read_port_1 &= !(0b1 << 2);
            },
            Button::P2Start => {
                self.port_state.read_port_1 &= !(0b1 << 1);
            },
            Button::P1Shoot => {
                self.port_state.read_port_1 &= !(0b1 << 4);
            },
            Button::P2Shoot => {
                self.port_state.read_port_2 &= !(0b1 << 4);
            },
            Button::P1Left => {
                self.port_state.read_port_1 &= !(0b1 << 5);
            },
            Button::P2Left => {
                self.port_state.read_port_2 &= !(0b1 << 5);
            },
            Button::P1Right => {
                self.port_state.read_port_1 &= !(0b1 << 6);
            },
            Button::P2Right => {
                self.port_state.read_port_2 &= !(0b1 << 6);
            },
            Button::Coin => {
                self.port_state.read_port_1 |= 0b1;
            },
            Button::NumLivesSwitch0 => {
                self.port_state.read_port_2 &= !(0b1);
            },
            Button::NumLivesSwitch1 => {
                self.port_state.read_port_2 &= !(0b1 << 1);
            },
            Button::Tilt => {
                self.port_state.read_port_2 &= !(0b1 << 2);
            },
            Button::BonusLife => {
                self.port_state.read_port_2 &= !(0b1 << 3);
            },
            Button::CoinInfo => {
                self.port_state.read_port_2 |= 0b1 << 7;
            },
        }
    }
}