alias t := test
alias c := clean
alias l := lint

test: build
    cd rust_compiler && mold -run cargo nextest run
    cd rust_compiler && mold -run cargo run -- test ../compiler_tests

build:
    cd rust_compiler && cargo build

clean:
    cd rust_compiler && cargo clean

lint:
    cd rust_compiler && cargo clippy
