use std::{collections::BTreeMap, io::{BufReader, BufWriter}};
use std::io::{self, prelude::*};

use crate::tree::HuffmanTree;

/// Information related to a byte in HuffmanCanonical.
struct SymbolInfo {
    /// Number of time this symbol appeared in the source.
    /// This is for diagnostic purposes.
    count: u64,
    /// Frequency of the symbol. This is for diagnostic purposes.
    weight: f64,
    /// Encoded representation in binary. In theory, a representation could be
    /// 255-bit long.  However, since the input length is limited to an u64,
    /// the longest possible representation is 63-bit long (and even reaching
    /// that would require a gigantic input length).  The representation is
    /// kept as a u128 to make shifting beyond a 64-bit length possible without
    /// any risk of overflow.
    repr: u128,
    /// Length in bits of the representation.
    nbits: u8,
}

/// Represent a canonical Huffman code.
pub struct HuffmanCanonical {
    hbook: BTreeMap<u8, SymbolInfo>,
    input_size: u64,
}

impl HuffmanCanonical {
    /// Read a stream and build a canonical Huffman code from it.
    pub fn from_stream<T: Read>(stream: T) -> io::Result<Self> {
        let tree = HuffmanTree::from_stream(stream)?;
        Ok(tree.into())
    }

    /// Entropy of the original stream.
    pub fn entropy(&self) -> f64 {
        -self
            .hbook
            .values()
            .map(|SymbolInfo { weight, .. }| weight * weight.log2())
            .sum::<f64>()
    }

    /// Size in bytes of encoded stream, not including tree description.
    pub fn bare_encoded_len(&self) -> u64 {
        let nbits: u64 = self.hbook.values()
            .map(|s| s.count * s.nbits as u64)
            .sum();
        let rem = nbits % 8 != 0;
        nbits / 8 + rem as u64
    }

    /// Size of encoded stream, including tree description.
    pub fn total_encoded_len(&self) -> u64 {
        let bare = self.bare_encoded_len();
        // header and number of decoded bytes
        bare + 256 + 8
    }

    /// Encode tree and number of bytes in the source.
    ///
    /// This forms the header of the encoded file.
    /// For now, simply dump the number of bits for each symbol.
    /// Other strategies could be used depending on which symbols are
    /// actually encoded.
    fn encode_header<W: Write>(&self, writer: &mut W) -> io::Result<()> {
        let tree_rep: Vec<u8> = (0..=u8::MAX)
            .into_iter()
            .map(|b| self.hbook.get(&b)
                 .map(|s| s.nbits as u8).unwrap_or(0))
            .collect();
        writer.write_all(&tree_rep)?;
        writer.write_all(&self.input_size.to_le_bytes())?;
        writer.flush()?;
        Ok(())
    }

    /// Encode a stream using the canonical Huffman code.
    ///
    /// This assumes input is the same as the one used to build the handbook in
    /// the first place!  The lib crate should only expose an API that takes a
    /// filename to do on disk, reading twice, OR takes a stream and keep it in
    /// memory.
    pub fn encode<R, W>(&self, input: R, output: W) -> io::Result<()>
    where
        R: Read,
        W: Write,
    {
        let mut input = BufReader::new(input);
        let mut output = BufWriter::new(output);

        self.encode_header(&mut output)?;

        // shift in the current byte being built
        let mut bit_shift = 0;
        // current byte being built, it is 128-bit long to
        // allow concatenation of a 64-bit long representation
        // without overflow
        let mut byte_buffer: u128 = 0;

        loop {
            // read bytes from the input
            let buf = input.fill_buf()?;
            let length = buf.len();
            if length == 0 {
                break;
            }

            for byte in buf {
                let SymbolInfo { repr, nbits, .. } = self.hbook[byte];
                byte_buffer += repr << bit_shift;
                bit_shift += nbits;
                while bit_shift >= 8 {
                    output.write_all(&[(byte_buffer % 256) as u8])?;
                    byte_buffer >>= 8;
                    bit_shift -= 8;
                }
            }

            input.consume(length);
        }
        // dump extraneous bits in the output
        output.write_all(&[byte_buffer as u8])?;
        output.flush()?;

        Ok(())
    }
}

/// Helper to build HuffmanCanonical, yields the list of (depth, byte, count).
fn explore(tree: HuffmanTree<u64>, depth: u8) -> Vec<(u8, u8, u64)> {
    match tree {
        HuffmanTree::Node { left, right, .. } => {
            let mut summary = explore(*left, depth + 1);
            summary.append(&mut explore(*right, depth + 1));
            summary
        }
        HuffmanTree::Leaf { value, byte } => {
            vec![(depth, byte, value)]
        }
    }
}

impl From<HuffmanTree<u64>> for HuffmanCanonical {
    fn from(tree: HuffmanTree<u64>) -> Self {
        let input_size = *tree.value();
        let tot_count = input_size as f64;
        let mut dbc = explore(tree, 0);
        dbc.sort_unstable(); // to produce canonical Huffman code

        let mut bmap = BTreeMap::new();

        let mut dbc_iter = dbc.into_iter();
        let (depth, byte, count) = dbc_iter.next().unwrap();
        let weight = count as f64 / tot_count;
        let mut repr = 0;
        bmap.insert(
            byte,
            SymbolInfo {
                count,
                weight,
                repr,
                nbits: depth,
            },
        );
        let mut prev_depth = depth;

        for (depth, byte, count) in dbc_iter {
            let weight = count as f64 / tot_count;
            // repr bits are reversed because decoding is done from the root of
            // the tree rather than the leaf and therefore the most significant
            // digit is needed first.
            repr += 1;
            repr <<= depth - prev_depth;
            prev_depth = depth;
            bmap.insert(
                byte,
                SymbolInfo {
                    count,
                    weight,
                    repr: repr.reverse_bits() >> (128 - depth),
                    nbits: depth,
                },
            );
        }
        Self {
            hbook: bmap,
            input_size
        }
    }
}
