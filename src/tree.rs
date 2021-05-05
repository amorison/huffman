use std::{array, hint::unreachable_unchecked};
use std::cmp::Reverse;
use std::ops::Add;
use std::collections::VecDeque;
use std::io::{self, prelude::*, BufReader};
use std::iter::FromIterator;

/// A HuffmanTree associating bytes to arbitrary values.
pub enum HuffmanTree<V> {
    Node {
        value: V,
        left: Box<HuffmanTree<V>>,
        right: Box<HuffmanTree<V>>,
    },
    Leaf {
        value: V,
        byte: u8,
    },
}

impl<V: PartialOrd> HuffmanTree<V> {
    /// The value of the HuffmanTree.
    ///
    /// This is either the value associated to a byte (Leaf), or
    /// the sum of children values (Node).
    pub fn value(&self) -> &V {
        match self {
            HuffmanTree::Node { value, .. } => value,
            HuffmanTree::Leaf { value, .. } => value,
        }
    }
}

impl HuffmanTree<u64> {
    /// Consumes a stream and build a HuffmanTree from it.
    pub fn from_stream<T: Read>(stream: T) -> io::Result<Self> {
        let mut reader = BufReader::new(stream);
        let mut counter = [0u64; 256];
        let mut something_read = false;
        loop {
            let buf = reader.fill_buf()?;
            let length = buf.len();
            if length == 0 {
                break;
            }
            something_read = true;
            for &byte in buf {
                counter[byte as usize] += 1;
            }
            reader.consume(length);
        }
        if !something_read {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "Empty file"));
        }
        let tree = array::IntoIter::new(counter)
            .enumerate()
            .filter_map(|(byte, count)| (count != 0).then(|| (count, byte as u8)))
            .collect();
        Ok(tree)
    }
}

/// This is a helper function to build a HuffmanTree.
///
/// It pops the smallest leaf/partial tree in both queues, favoring leaves to
/// minimize unbalance in the resulting tree.
fn pop_smallest<V: PartialOrd>(
    leaves: &mut Vec<HuffmanTree<V>>,
    nodes: &mut VecDeque<HuffmanTree<V>>,
) -> HuffmanTree<V> {
    if let Some(leaf) = leaves.pop() {
        if let Some(node) = nodes.pop_back() {
            if leaf.value() <= node.value() {
                nodes.push_back(node);
                leaf
            } else {
                leaves.push(leaf);
                node
            }
        } else {
            leaf
        }
    } else {
        nodes.pop_back().unwrap()
    }
}

impl<V> FromIterator<(V, u8)> for HuffmanTree<V>
where
    V: Copy,
    V: Ord,
    V: Add<Output = V>,
{
    /// Builds a HuffmanTree from a collection of bytes and their values.
    ///
    /// The process to build a HuffmanTree uses two queues, one containing
    /// leaves (essentially a (value, byte) tuple), the other containing
    /// partial tree. Queues are ordered by decreasing values (and then
    /// bytes to form canonical encoding).  Both elements with the lowest value
    /// are combined to form a partial tree and pushed in the relevant queue
    /// until only one tree remains.
    fn from_iter<T: IntoIterator<Item = (V, u8)>>(iter: T) -> Self {
        let mut leaves: Vec<_> = iter
            .into_iter()
            .map(|(value, byte)| HuffmanTree::Leaf { value, byte })
            .collect();
        if leaves.len() == 1 {
            return leaves.pop().unwrap()
        }
        assert!(!leaves.is_empty());
        leaves.sort_unstable_by_key(|l| {
            match l {
                HuffmanTree::Leaf { value, byte } => {
                    // sort by decreasing value then byte
                    Reverse((*value, *byte))
                },
                _ => unsafe {
                    // SAFETY: leaves only contains Leaf variants
                    unreachable_unchecked()
                },
            }
        });
        let mut nodes = VecDeque::with_capacity(leaves.len() / 2);
        while nodes.len() != 1 || !leaves.is_empty() {
            // group two lightest trees in a new tree
            let left = Box::new(pop_smallest(&mut leaves, &mut nodes));
            let right = Box::new(pop_smallest(&mut leaves, &mut nodes));
            let value = *left.value() + *right.value();
            let new_node = HuffmanTree::Node { value, left, right };
            nodes.push_front(new_node);
        }
        nodes.pop_back().unwrap()
    }
}
