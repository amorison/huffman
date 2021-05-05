use std::io::{self, BufReader, BufWriter, prelude::*};
use std::ops::Add;

use crate::tree::HuffmanTree;

/// This is responsible of decoding streams of bits, visiting a Huffman tree.
struct SymbolFactory<'a, V> {
    /// The entire Huffman tree representing the canonical code.
    root: &'a HuffmanTree<V>,
    /// The current position in said tree, this is always a Node.
    tree: &'a HuffmanTree<V>,
    /// Number of bytes left to decode.
    n_symbols: u64,
}

impl<'a, V> SymbolFactory<'a, V> {
    /// Create a SymbolFactory expecting a given number of symbols to decode.
    fn new(tree: &'a HuffmanTree<V>, n_symbols: u64) -> Self {
        Self {
            root: tree,
            tree: tree,
            n_symbols
        }
    }

    /// Whether all symbols were decoded.
    fn is_finished(&self) -> bool {
        self.n_symbols == 0
    }

    /// Advance in the tree with the given encoded bytes, yielding the
    /// decoded bytes.
    fn decode_bytes(&mut self, bytes: &[u8]) -> Vec<u8> {
        let mut output = Vec::new();
        if self.is_finished() {
            return output;
        }
        'byte_loop: for mut byte in bytes.iter().cloned() {
            for _ in 0..8 {
                self.tree = match self.tree {
                    HuffmanTree::Node { left, right, .. } => {
                        if byte % 2 == 0 { left } else { right }
                    }
                    HuffmanTree::Leaf { .. } => {
                        unreachable!()
                    }
                };
                byte /= 2;
                if let HuffmanTree::Leaf { byte, .. } = self.tree {
                    output.push(*byte);
                    self.tree = self.root;
                    self.n_symbols -= 1;
                    if self.is_finished() {
                        break 'byte_loop;
                    }
                };
            }
        }
        output
    }
}

/// Represent the depth of a symbol in a canonical Huffman tree.
///
/// It implements Add and Ord so that HuffmanTree::FromIterator<(Depth, u8)>
/// yields the canonical tree.
#[derive(Copy, Clone, Eq, PartialEq)]
struct Depth(u8);

impl Add for Depth {
    type Output = Depth;

    fn add(self, rhs: Self) -> Self::Output {
        assert!(self == rhs);
        Depth(self.0.checked_sub(1).unwrap())
    }
}

impl Ord for Depth {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.0.cmp(&self.0)
    }
}

impl PartialOrd for Depth {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// Returns the canonical tree producing a given header.
fn tree_from_header(header: &[u8]) -> HuffmanTree<Depth> {
    header.iter()
        .enumerate()
        .filter_map(|(byte, &depth)| {
            (depth != 0).then(|| (Depth(depth), byte as u8))
        })
        .collect()
}

/// Decode a given stream, assuming it has a well formed and
/// meaningful header describing the used Huffman code.
pub fn decode<R, W>(input: R, output: W) -> io::Result<()>
where
    R: Read,
    W: Write,
{
    let mut input = BufReader::new(input);
    let header_len = 256;
    let mut header = vec![0; header_len];
    if header_len != input.read(&mut header)? {
        return Err(io::Error::new(io::ErrorKind::InvalidData,
                                  "Could not read header"));
    }
    let tree = tree_from_header(&header);
    let mut n_symbols = [0; 8];
    if 8 != input.read(&mut n_symbols)? {
        return Err(io::Error::new(io::ErrorKind::InvalidData,
                                  "Could not read header"));
    }
    let n_symbols = u64::from_le_bytes(n_symbols);
    let mut factory = SymbolFactory::new(&tree, n_symbols);
    let mut output = BufWriter::new(output);
    loop {
        let buf = input.fill_buf()?;
        let length = buf.len();
        if length == 0 {
            break;
        }
        let decoded = factory.decode_bytes(buf);
        output.write(&decoded)?;
        input.consume(length);
    }
    output.flush()?;
    if !factory.is_finished() {
        return Err(io::Error::new(io::ErrorKind::UnexpectedEof,
                                  "Missing symbols to decode"));
    }
    Ok(())
}
