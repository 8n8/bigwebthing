use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn bwt(raw: Vec<u8>) -> Vec<u8> {
	let mut result = vec![0; raw.len()];
	for i in 0..raw.len() {
		result[i] = raw[i] + 1
	}
	result
}
