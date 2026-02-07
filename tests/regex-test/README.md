Source code used to create `/wizer/tests/regex_test.wasm`.

Rebuild with:

```console
cargo build --release --target wasm32-wasip1 -p regex-test
cp target/wasm32-wasip1/release/regex_test.wasm tests/regex_test.wasm
```
