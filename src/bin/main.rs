use std::env;
use std::process;
use std::fs::{File, OpenOptions};
use std::io;

use huffman::HuffmanCanonical;
use huffman::decode;

/// Manages CLI arguments.
struct Config {
    /// Name of the file to encode.  If the filename ends with
    /// ".huffman", the file is decoded.
    filename: String,
}

impl Config {
    pub fn new<T, I>(args: T) -> Result<Config, String>
    where
        T: IntoIterator<Item = I>,
        I: Into<String>,
    {
        let mut args = args.into_iter();
        args.next().ok_or("No argument passed in")?;
        let usage = "Usage: huffman <filename>";
        let filename = args
            .next()
            .ok_or(format!("Missing filename. {}", usage))?
            .into();
        if args.next().is_some() {
            return Err(format!("Too many arguments. {}", usage));
        }
        Ok(Config { filename })
    }
}

/// Produce an encoded file from a regular file.
fn encoding_task(config: Config) -> io::Result<()> {
    let file = File::open(&config.filename)?;
    let hfcanonical = HuffmanCanonical::from_stream(file)?;

    println!("Entropy: {}", hfcanonical.entropy());

    let out_name = format!("{}.huffman", config.filename);
    let input = File::open(&config.filename)?;
    let output = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(out_name)?;

    hfcanonical.encode(input, output)?;

    Ok(())
}

/// Produce an regular file from an encoded file.
fn decoding_task(config: Config) -> io::Result<()> {
    let out_name = format!("{}.decoded", config.filename);
    let input = File::open(&config.filename)?;
    let output = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(out_name)?;

    decode(input, output)?;

    Ok(())
}

fn main() {
    let config = Config::new(env::args()).unwrap_or_else(|err| {
        eprintln!("{}", err);
        process::exit(1);
    });
    let task = if config.filename.ends_with(".huffman") {
        decoding_task
    } else {
        encoding_task
    };
    if let Err(e) = task(config) {
        eprintln!("Runtime issue: {}", e);
        process::exit(1);
    }
}
