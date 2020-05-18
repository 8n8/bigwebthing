use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn big_web_thing(s: String) -> Vec<u8> {
    encode_small_string(&s[..])
}

fn encode_small_string(s: &str) -> Vec<u8> {
    let mut result: Vec<u8> = Vec::new();
    result.push(1);
    let as_bytes = s.as_bytes();
    let length = as_bytes.len();
    for byte in encode_to_i32(length).iter() {
        result.push(byte.clone());
    }
    for byte in as_bytes {
        result.push(byte.clone());
    }
    return result;
}

fn encode_to_i32(number: usize) -> Vec<u8> {
    let mut result: Vec<u8> = vec![0, 0, 0, 0];
    let as_u32 = number as u32;
    for i in 0..3 {
        result[i] = ((as_u32 >> (i * 8)) & (0xff as u32)) as u8;
    }
    return result;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encode_to_i32_0() {
        let expected: Vec<u8> = vec![0, 0, 0, 0];
        let input: usize = 0;
        assert_eq!(encode_to_i32(input), expected);
    }

    #[test]
    fn encode_to_i32_2() {
        let expected: Vec<u8> = vec![1, 0, 0, 0];
        let input: usize = 1;
        assert_eq!(encode_to_i32(input), expected);
    }

    #[test]
    fn encode_to_i32_256() {
        let expected: Vec<u8> = vec![1, 1, 0, 0];
        let input: usize = 257;
        assert_eq!(encode_to_i32(input), expected);
    }
}
