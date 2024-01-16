# The Design and Implemntation of An Assembler and Linker for Teaching and Experiments.

The repository is the official implementation of [The Design and Implemntation of An Assembler and Linker for Teaching and Experiments](./README.md). The paper presents the design and implementation of an assembler and linker on the x86 Linux platform for teaching experiments in computer architecture and program design. The assembler uses AT&T syntax and the linker can perform static linking of several target files. The assembler supports 158 different instructions, or a total of 398 different instructions if taking the differences of operand format into account. 

## Requirements

Install Rust:
```
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

## Usage

1. Build the assmbler and linker:
   ```
   cargo build
   ```

2. Then add `./target/debug/assembler` and `./target/debug/linker` to PATH.
3. The input parameters to the assembler take the following format:
   ```
   assembler -o <output-file> <input-file>
   ```
4. The input parameters to the linker take the following format:
   ```
   linker -o <output-file> <input-file1> <input-file2> ...
   ```