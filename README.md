# huffman

This CLI tool is capable of encoding/decoding files with a canonical Huffman
code.

```
$ ./huffman some_file
```

encodes the file, producing `some_file.huffman`.

```
$ ./huffman some_file.huffman
```

decodes the file, producing `some_file.huffman.decoded`.
