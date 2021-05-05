use std::io;
use huffman::{HuffmanCanonical, decode};

fn reciprocal_encode_decode(original: &[u8]) -> io::Result<()> {
    let canonical = HuffmanCanonical::from_stream(original)?;
    let mut encoded = Vec::new();
    canonical.encode(original, &mut encoded)?;
    let mut decoded = Vec::new();
    decode(&encoded[..], &mut decoded)?;
    assert_eq!(original, decoded);
    Ok(())
}

#[test]
fn reciprocal_az() -> io::Result<()> {
    let original = "abcdefghijklmnopqrstuvwxyz";
    reciprocal_encode_decode(original.as_bytes())
}

#[test]
fn reciprocal_abaca_bed() -> io::Result<()> {
    let original = "A_DEAD_DAD_CEDED_A_BAD_BABE_A_BEADED_ABACA_BED";
    reciprocal_encode_decode(original.as_bytes())
}
