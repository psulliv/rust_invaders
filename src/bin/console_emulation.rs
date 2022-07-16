fn main() {
    #[cfg(target_arch = "x86_64")]
    {
        let machine = invaders::machine::MachineState::new();
        invaders::emulation_loop(machine);
    }
    ()
}
