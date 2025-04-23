# Monkey

A Rust implementation of the [Monkey language](https://monkeylang.org/)'s interpreter and compiler. Playground [here](https://austionian.github.io/monkey/).

Added features:

- Support for logical operators, `&&` and `||`

## Instructions

Build the app:

```
cargo build --release
```

To start the repl using the compiler/vm:

```
./target/release/repl
```

To start the repl using the interpreter:

```
./target/release/repl eval
```

To execute a `.monkey` file, pass a `--path`

```
./target/release/repl --path monkey_examples/fibonacci.monkey
```

## Benchmark between compiler and interpreter:

Computing the 25<sup>th</sup> fibonacci integer.
![Screenshot From 2025-04-19 23-12-35](https://github.com/user-attachments/assets/233e90f5-260b-4a95-b0c7-1215709e0e6b)
