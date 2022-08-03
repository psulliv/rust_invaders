use wasm_bindgen::JsCast;

const AUDIO_DIR: &str = "audio/";
const AUDIO_EXT: &str = ".wav";

const SOUND_FILENAMES: [&str; 10] = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];

pub fn load_audio() {
    let document = web_sys::window().unwrap().document().unwrap();
    for filename in SOUND_FILENAMES {
        let audio_element = document
            .create_element("audio")
            .unwrap()
            .dyn_into::<web_sys::HtmlAudioElement>()
            .unwrap();
        audio_element.set_src(&format!("{}{}{}", AUDIO_DIR, filename, AUDIO_EXT));
        audio_element.set_id(&filename);
        document
            .body()
            .unwrap()
            .append_child(&audio_element)
            .unwrap();
    }
}

pub fn play_sound(element_id: &str) {
    let document = web_sys::window().unwrap().document().unwrap();
    let sound = document
        .get_element_by_id(element_id)
        .unwrap()
        .dyn_into::<web_sys::HtmlAudioElement>()
        .unwrap();
    sound.play().ok();
}

pub fn set_loop(element_id: &str, enabled: bool) {
    let document = web_sys::window().unwrap().document().unwrap();
    let sound = document
        .get_element_by_id(element_id)
        .unwrap()
        .dyn_into::<web_sys::HtmlAudioElement>()
        .unwrap();
    sound.set_loop(enabled);
}
