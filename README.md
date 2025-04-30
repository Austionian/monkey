# Monkey

A Rust implementation of the [Monkey language](https://monkeylang.org/)'s interpreter and compiler.

### [Playground](https://austionian.github.io/monkey/)

Added features:

- Comments `// This is a comment`
- Logical operators, `&&` and `||`
- Loops, `loop` and `break`
- Mutate variables:

```
let foo = 1;
foo = foo + 1;
foo; //2
```

- Postfix operators on variables, `foo++;` and `bar--;`

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
./target/release/repl --mode eval
```

To execute a `.monkey` file with the compiler/vm:

```
./target/release/repl monkey_examples/fibonacci.monkey
```

To execute a `.monkey` file with the interpreter:

```
./target/release/repl monkey_examples/fibonacci.monkey --mode eval
```

## Benchmark between compiler and interpreter:

Computing the 25<sup>th</sup> fibonacci integer.
![Screenshot From 2025-04-19 23-12-35](https://github.com/user-attachments/assets/233e90f5-260b-4a95-b0c7-1215709e0e6b)
