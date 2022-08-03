use wasm_bindgen::JsCast;

const AUDIO_DIR: &str = "audio/";
const AUDIO_EXT: &str = ".wav";

const SOUND_FILENAMES: [&str; 10] = [
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
];

pub fn load_audio() {
    let document = web_sys::window().unwrap().document().unwrap();
    for filename in SOUND_FILENAMES {
        let audio_element = document
            .create_element("audio")
            .unwrap()
            .dyn_into::<web_sys::HtmlAudioElement>()
            .unwrap();
        audio_element.set_src(&format!("{}{}{}", AUDIO_DIR, filename, AUDIO_EXT));
        document.body().unwrap().append_child(&audio_element).unwrap();
    }
}
