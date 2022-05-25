# `rename-item`

Procedural macro to rename items on declaration, according to different case styles.

This crate provides the [`macro@rename`] attribute-like macro that changes the name of the item it
is applied to. In order to refer to this changed name, this crate provides the [`renamed!`]
function-like macro.

Both macros accept the same four named arguments for specifying the generated name:

- The `name` argument specifies the individual components the generated name will be made up of. It
  can be a single string literal, as in `name = "foo"`, or a list of string literals or identifiers,
  as in `name("foo", bar)`. The components are split further into individual words: `name("fooBar")`
  yields the same result as `name("foo", "bar")`. This split is done using the [`heck`] crate and
  follows its definition of words boundaries. The `name` argument is always required.

- The `case` argument specifies the case style the generated name will follow. It has to be a string
  literal, as in `case = "snake"`. This case conversion is performed on the words extracted from the
  `name` argument; the `prefix` and `suffix` arguments explained below are not affected by the case
  conversion. Refer to the table below for a list of supported case styles. The `case` argument is
  always required for the [`renamed!`] macro; for the [`macro@rename`] macro a default case style
  can be inferred from the annotated item's type.

- The `prefix` argument specifies an additional string literal that is prepended to the generated
  name. This is done after case conversion, so that the prefix string remains fixed independent of
  the target case style. The `prefix` argument is optional and defaults to the empty string.

- The `suffix` argument specifies an additional string literal that is appended to the generated
  name. This is done after case conversion, so that the suffix string remains fixed independent of
  the target case style. The `suffix` argument is optional and defaults to the empty string.

The following case styles are supported:

| Case style        | `case =`         | Example   |
| ----------------- | ---------------- | --------- |
| Upper camel case  | `"upper_camel"`  | `FooBar`  |
| Lower camel case  | `"lower_camel"`  | `fooBar`  |
| Snake case        | `"snake"`        | `foo_bar` |
| Shouty snake case | `"shouty_snake"` | `FOO_BAR` |

This crate is mainly useful when writing declarative macros, as declarative macros cannot create new
identifiers. Additionally, this crate allows declarative macros to adapt a given name to various
case styles, useful when the macro defines different kinds of items.

## Examples

```rust
use rename_item::{rename, renamed};

// This function has the name `_return_five`
#[rename(name = "return-five", prefix = "_")]
fn foo() -> i32 {
    5
}

assert_eq!(_return_five(), 5);

// This obtains the name of the function using the macro arguments from above, and calls it
let five = renamed!(name = "return-five", case = "snake", prefix = "_")();
assert_eq!(five, 5);
```

Here's a concrete example, a declarative macro that defines setters for struct members:

```rust
macro_rules! struct_with_setters {
    (
        // Match a normal struct definition
        struct $name:ident {
            $($member:ident: $ty:ty),* $(,)?
        }
    ) => {
        // Emit the same struct definition
        struct $name {
            $($member: $ty,)*
        }

        // Additionally emit an impl block with a setter function for each member
        impl $name {
            $(
                #[::rename_item::rename(name($member), prefix = "set_")]
                fn foo(&mut self, val: $ty) {
                    self.$member = val;
                }
            )*
        }
    };
}

struct_with_setters! {
    struct Highscore {
        score:  i32,
        player: String,
    }
}

let mut h = Highscore {
    score:  42,
    player: "Hackerman".into(),
};
h.set_score(9001);
assert_eq!(h.score, 9001);
```

## License

Licensed under either of [MIT License](LICENSE-MIT) or [Apache License, Version 2.0](LICENSE-APACHE)
at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in
this crate by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without
any additional terms or conditions.
