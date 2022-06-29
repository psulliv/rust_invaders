use std::fs;
use std::io::prelude::*;

pub fn create_invaders_memory_space() -> Result<Vec<u8>, std::io::Error> {
    //!Memory map:
    //!	ROM
    //!	$0000-$07ff:	invaders.h
    //!	$0800-$0fff:	invaders.g
    //!	$1000-$17ff:	invaders.f
    //!	$1800-$1fff:	invaders.e
    //!
    //!	RAM
    //!	$2000-$23ff:	work RAM
    //!	$2400-$3fff:	video RAM
    //!
    //!	$4000-:		RAM mirror
    let mut zip_arch = zip::ZipArchive::new(fs::File::open("invaders.zip")?)?;
    let mut invaders_mem: Vec<u8> = Vec::with_capacity(0x1fff);

    let mut invaders_h: Vec<u8> = Vec::new();
    zip_arch
        .by_name("invaders.h")?
        .read_to_end(&mut invaders_h)
        .expect("Failed to read invaders.h from zip file");
    let mut invaders_g: Vec<u8> = Vec::new();
    zip_arch
        .by_name("invaders.g")?
        .read_to_end(&mut invaders_h)
        .expect("Failed to read invaders.g from zip file");
    let mut invaders_f: Vec<u8> = Vec::new();
    zip_arch
        .by_name("invaders.f")?
        .read_to_end(&mut invaders_h)
        .expect("Failed to read invaders.f from zip file");
    let mut invaders_e: Vec<u8> = Vec::new();
    zip_arch
        .by_name("invaders.e")?
        .read_to_end(&mut invaders_h)
        .expect("Failed to read invaders.e from zip file");

    invaders_mem.append(&mut invaders_h);
    invaders_mem.append(&mut invaders_g);
    invaders_mem.append(&mut invaders_f);
    invaders_mem.append(&mut invaders_e);
    Ok(invaders_mem)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    #[test]
    fn verify_invader_bytes() {
        let mut hasher = DefaultHasher::new();
        create_invaders_memory_space().unwrap().hash(&mut hasher);
        let hashed_invaders_memory = hasher.finish();
        assert_eq!(3389242380157347933, hashed_invaders_memory);
    }
}
