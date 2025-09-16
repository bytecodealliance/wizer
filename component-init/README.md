# Pre-initialize WebAssembly Components

[Wizer][] demonstrated that WebAssembly modules can be made to start up
faster by pre-initializing them. `component-init` does the same thing,
but for the WebAssembly Component Model.

[Wizer]: https://github.com/bytecodealliance/wizer/

`component-init` is a Rust library that can be used in other projects.
(One prominent example is [componentize-py][].) To use it in your
project, you additionally need a WebAssembly runtime that supports the
Component Model; [Wasmtime][] is known to work.

[componentize-py]: https://github.com/bytecodealliance/componentize-py
[Wasmtime]: https://wasmtime.dev/

# Design rationale

To pre-initialize a WebAssembly program, you need to do three things:

1. Run its initialization function.
2. Snapshot any resulting changes to its internal mutable state.
3. Emit a new WebAssembly program with the updated mutable state.

Step 1 is easy and any runtime will do. Step 3 is a little tedious but
it just involves copying the original program mostly unchanged except
for updating data sections and such, so it's not too hard.

The challenge is in step 2: How do you refer to the internal state from
outside the program? Some state is explicitly exported from a module or
component, so that's easy to access via any runtime, but what about the
rest?

To solve this, both Wizer and component-init insert an extra step at the
beginning: Generate a new WebAssembly program that exports every piece
of mutable state (e.g. memories and globals) using new names, and
remember those names to look them up again during snapshotting. The
initialization function is then run in that "instrumented" version of
the original program.

Unlike core WebAssembly modules, WebAssembly components can't directly
export globals or memories, so component-init adds an exported accessor
function for each instead. The effect is essentially the same as Wizer's
use of exports though.

But component-init adds an additional wrinkle: Although Wizer relies on
Wasmtime as its runtime, component-init requires that whoever uses it
provide a suitable component runtime. This could be Wasmtime but doesn't
have to be.

So the caller needs to provide a callback function which will be given a
byte array containing a component. It then should run the initialization
function of that component, and return an object that component-init can
use to access various types of named exports from the component.

Given that callback, component-init can take care of the rest of the
process: instrumenting the component beforehand, and extracting the
state and writing a new component afterwards.
