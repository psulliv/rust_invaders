fn main() {
    let build_enabled = std::env::var("BUILD_ENABLED")
        .map(|v| v == "1")
        .unwrap_or(true);
    if build_enabled {
        cc::Build::new().file("src/8080emu.c").compile("8080emu.a");
    }
}
