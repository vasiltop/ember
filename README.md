# Ember

Ember is a simple interpreter programming language that follows the fundamental syntax of many popular programming languages.

[![Test Status](https://github.com/vasiltop/ember/workflows/test/badge.svg)](https://github.com/vasiltop/ember/actions)

## Installation

```bash
git clone https://github.com/vasiltop/ember
cd ember
cargo build --release
```

## Usage

```bash
ember <PATH>
```

## Examples

Code examples can be found in the examples directory.

## Variables

```rust
let name = "Ember";
```

## Control Flow

```rust
if x == 1 && true {
    x = 4;
} else {
    x = 7;
}

for (let i = 0; i < 10; i++) {
    print! i;
}

let i = 0;
while i < 10 {
    i = i + 1;
}
```

## Functions

```rust
fn add(a, b) {
    return a + b;
}

let added = add(4, 2);
```
