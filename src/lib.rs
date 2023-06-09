#![doc = include_str!("../README.md")]
#![forbid(unsafe_code)]
#![warn(clippy::missing_docs_in_private_items)]

use darling::{ast::NestedMeta, FromMeta};
use heck::{ToLowerCamelCase, ToShoutySnakeCase, ToSnakeCase, ToUpperCamelCase};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::ToTokens;
use syn::{
    parse::{discouraged::Speculative, Parse},
    parse_macro_input, parse_str, ForeignItem, Ident, Item, Lit, Meta,
};

/// Changes the name of the annotated item.
///
/// This macro changes the name of an item, which might make it difficult to refer to this item
/// later. The [`renamed!`] macro can be used to obtain the new name of the item.
///
/// The name is given by a mix of string literals and identifiers, which are concatenated and
/// adjusted to a given case style. Fixed prefix and suffix strings can also be provided, and will
/// not be adjusted to the case style. For further information on how names are generated, refer to
/// the [module-level documentation](self).
///
/// The target case style can be omitted. In that case the default case style for the item's type
/// will be used: `snake_case` for functions and modules, `SHOUTY_SNAKE_CASE` for constants and
/// statics, and `UpperCamelCase` for types and traits.
///
/// # Examples
///
/// ```
/// # use rename_item::rename;
/// #
/// #[rename(name = "my-constant")]
/// const foo: u32 = 1;
/// assert_eq!(MY_CONSTANT, 1);
///
/// #[rename(name(my, "constant"), case = "upper_camel", prefix = "_")]
/// const foo: u32 = 2;
/// assert_eq!(_MyConstant, 2);
/// ```
#[proc_macro_attribute]
pub fn rename(args: TokenStream, item: TokenStream) -> TokenStream {
    // Parse attribute and item
    let args = match NestedMeta::parse_meta_list(args.into()) {
        Ok(v) => v,
        Err(e) => {
            return e.into_compile_error().into();
        }
    };
    let mut item = parse_macro_input!(item as InputItem);

    // Convert macro input to target name
    let name = MacroInput::from_list(&args).and_then(|input| input.into_name(Some(&item)));

    // Apply target name to the item
    let toks = name.and_then(|name| {
        let ident = Ident::new(&name, Span::call_site());
        set_ident(&mut item, ident)?;
        Ok(item.into_token_stream())
    });

    // Handle errors
    match toks {
        Ok(toks) => toks,
        Err(err) => err.write_errors(),
    }
    .into()
}

/// Expands to the name of an item.
///
/// This macro expands to the name specified by the macro arguments. To apply this name to an item,
/// use the [`macro@rename`] macro.
///
/// The name is given by a mix of string literals and identifiers, which are concatenated and
/// adjusted to a given case style. Fixed prefix and suffix strings can also be provided, and will
/// not be adjusted to the case style. For further information on how names are generated, refer to
/// the [module-level documentation](self).
///
/// The prefix and suffix strings can be used to extend the generated name beyond a single
/// identifier. In this way, arbitrary tokens can be inserted before or after the generated name.
/// This is useful for surrounding the generated name with additional path components (e.g.
/// `Self::`) or expressions (e.g. `1+`).
///
/// # Examples
///
/// ```
/// # use rename_item::renamed;
/// #
/// # let foo_bar = 1;
/// assert_eq!(renamed!(case = "snake", name = "foo-bar"), foo_bar);
///
/// # let fooBar1 = 2;
/// assert_eq!(
///     renamed!(case = "lower_camel", name(foo, "bar"), suffix = "1"),
///     fooBar1
/// );
///
/// assert_eq!(
///     renamed!(case = "snake", name = "foo-bar", prefix = "1+"),
///     1 + foo_bar
/// );
/// ```
///
/// The case style cannot be inferred from the item's type and must always be specified. The
/// following code fails to compile:
///
/// ```compile_fail
/// # use rename_item::renamed;
/// renamed!(name = "foo")
/// ```
#[proc_macro]
pub fn renamed(args: TokenStream) -> TokenStream {
    // Parse attribute
    let args = match NestedMeta::parse_meta_list(args.into()) {
        Ok(v) => v,
        Err(e) => {
            return e.into_compile_error().into();
        }
    };

    // Convert macro input to target name
    let name = MacroInput::from_list(&args).and_then(|input| input.into_name(None));

    // Convert name to token stream and handle errors
    match name {
        Ok(name) => match parse_str(&name) {
            Ok(toks) => toks,
            Err(err) => err.into_compile_error(),
        },
        Err(err) => err.write_errors(),
    }
    .into()
}

/// Input to the [`rename`] and [`renamed`] macros
#[derive(Debug, FromMeta)]
struct MacroInput {
    /// Case style used to build the output string
    #[darling(default)]
    case:   Option<CaseStyle>,
    /// Individual words of the name
    name:   Words,
    /// Prefix for the output string
    #[darling(default)]
    prefix: String,
    /// Suffix for the output string
    #[darling(default)]
    suffix: String,
}

/// Case style
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum CaseStyle {
    /// Upper camel case: `FooBar`
    UpperCamel,
    /// Lower camel case: `fooBar`
    LowerCamel,
    /// Snake case: `foo_bar`
    Snake,
    /// Shouty snake case: `FOO_BAR`
    ShoutySnake,
}

impl FromMeta for CaseStyle {
    fn from_string(value: &str) -> darling::Result<Self> {
        // Convert string to case style. Case styles must be specified in snake case.
        match value {
            "upper_camel" => Ok(Self::UpperCamel),
            "lower_camel" => Ok(Self::LowerCamel),
            "snake" => Ok(Self::Snake),
            "shouty_snake" => Ok(Self::ShoutySnake),
            _ => Err(darling::Error::unknown_value(value)),
        }
    }
}

/// Individual words of the name
#[derive(Debug)]
struct Words(Vec<String>);

impl FromMeta for Words {
    fn from_list(words: &[NestedMeta]) -> darling::Result<Self> {
        // Convert from list of string literals or identifiers, as in `name("foo", bar)`

        let mut names = Vec::new();
        let mut errors = darling::Error::accumulator();

        // Convert all words to strings
        for word in words {
            // Handle string literals
            if let NestedMeta::Lit(Lit::Str(s)) = word {
                names.push(s.value());
                continue;
            }
            // Handle identifiers
            if let NestedMeta::Meta(Meta::Path(p)) = word {
                if let Some(ident) = p.get_ident() {
                    names.push(ident.to_string());
                    continue;
                }
            }

            // Otherwise, emit an error
            errors.push(
                darling::Error::custom("Expected string literal or identifier").with_span(word),
            );
        }

        errors.finish_with(Self(names))
    }

    fn from_string(value: &str) -> darling::Result<Self> {
        // Convert from single string value, as in `name = "foo"`
        Ok(Self(vec![value.to_owned()]))
    }
}

impl MacroInput {
    /// Concatenates words and adjusts to case style
    fn into_name(self, item: Option<&InputItem>) -> darling::Result<String> {
        // Infer default case style from type of `item`
        let case = self.case.or(match item {
            Some(InputItem::Regular(regular)) => match regular {
                Item::Fn(_) | Item::Mod(_) => Some(CaseStyle::Snake),

                Item::Enum(_)
                | Item::Struct(_)
                | Item::Trait(_)
                | Item::TraitAlias(_)
                | Item::Type(_)
                | Item::Union(_) => Some(CaseStyle::UpperCamel),

                Item::Const(_) | Item::Static(_) => Some(CaseStyle::ShoutySnake),

                _ => None,
            },

            Some(InputItem::Foreign(foreign)) => match foreign {
                ForeignItem::Fn(_) => Some(CaseStyle::Snake),

                ForeignItem::Type(_) => Some(CaseStyle::UpperCamel),

                ForeignItem::Static(_) => Some(CaseStyle::ShoutySnake),

                _ => None,
            },

            _ => None,
        });
        let case =
            case.ok_or_else(|| darling::Error::custom("Unable to infer default case style"))?;

        // Concatenate words. Insert `_` to ensure word boundary between words.
        let name = self.name.0.join("_");

        // Convert to case style
        let name = match case {
            CaseStyle::UpperCamel => name.to_upper_camel_case(),
            CaseStyle::LowerCamel => name.to_lower_camel_case(),
            CaseStyle::Snake => name.to_snake_case(),
            CaseStyle::ShoutySnake => name.to_shouty_snake_case(),
        };

        // Prepend prefix and append suffix
        Ok([self.prefix, name, self.suffix].concat())
    }
}

/// Sets the identifier of an [`InputItem`]
fn set_ident(item: &mut InputItem, ident: Ident) -> darling::Result<()> {
    match *item {
        InputItem::Regular(ref mut regular) => match regular {
            Item::Const(ref mut i) => i.ident = ident,
            Item::Enum(ref mut i) => i.ident = ident,
            Item::ExternCrate(ref mut i) => i.ident = ident,
            Item::Fn(ref mut i) => i.sig.ident = ident,
            Item::Mod(ref mut i) => i.ident = ident,
            Item::Static(ref mut i) => i.ident = ident,
            Item::Struct(ref mut i) => i.ident = ident,
            Item::Trait(ref mut i) => i.ident = ident,
            Item::TraitAlias(ref mut i) => i.ident = ident,
            Item::Type(ref mut i) => i.ident = ident,
            Item::Union(ref mut i) => i.ident = ident,

            _ => {
                return Err(darling::Error::custom("Unsupported item type"));
            }
        },

        InputItem::Foreign(ref mut foreign) => match foreign {
            ForeignItem::Fn(ref mut i) => i.sig.ident = ident,
            ForeignItem::Static(ref mut i) => i.ident = ident,
            ForeignItem::Type(ref mut i) => i.ident = ident,

            _ => {
                return Err(darling::Error::custom("Unsupported foreign-item type"));
            }
        },
    }
    Ok(())
}

/// An item we can rename: either a regular or a foreign item
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum InputItem {
    /// A regular item, not inside an `extern` block
    Regular(Item),
    /// A foreign item, inside an `extern` block
    Foreign(ForeignItem),
}

impl Parse for InputItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ahead = input.fork();

        if let Some(item) = Item::parse(&ahead)
            .ok()
            .filter(|it| !matches!(it, Item::Verbatim(_)))
        {
            input.advance_to(&ahead);
            Ok(Self::Regular(item))
        } else if let Some(item) = ForeignItem::parse(input)
            .ok()
            .filter(|it| !matches!(it, ForeignItem::Verbatim(_)))
        {
            Ok(Self::Foreign(item))
        } else {
            Err(input.error("unsupported item type"))
        }
    }
}

impl ToTokens for InputItem {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Regular(item) => item.to_tokens(tokens),
            Self::Foreign(item) => item.to_tokens(tokens),
        }
    }
}

/// Additional compile-fail tests, as doctests for convenience:
///
/// The `name` argument does not take a list of lists:
///
/// ```compile_fail
/// # use rename_item::rename;
/// #[rename(name(foo, foo(bar)))]
/// fn foo() {}
/// ```
///
/// The `name` argument is required:
///
/// ```compile_fail
/// # use rename_item::rename;
/// #[rename(case = "snake")]
/// fn foo() {}
/// ```
///
/// The `case` argument must be a string literal:
///
/// ```compile_fail
/// # use rename_item::rename;
/// #[rename(name = "foo", case(snake))]
/// fn foo() {}
/// ```
///
/// The `case` argument must be one of the supported cases:
///
/// ```compile_fail
/// # use rename_item::rename;
/// #[rename(name = "foo", case = "nonexistent")]
/// fn foo() {}
/// ```
///
/// Also test renaming of all possible types of items:
///
/// ```
/// use rename_item::rename;
///
/// extern "C" {
///     // Foreign fn
///     #[rename(name = "my-ffn")]
///     fn foo() -> i32;
///
///     // Foreign static
///     #[rename(name = "my-fs")]
///     static foo: i32;
/// }
///
/// // Const
/// #[rename(name = "my-const")]
/// const foo: i32 = 1;
/// assert_eq!(MY_CONST, 1);
///
/// // Enum
/// #[rename(name = "my-enum")]
/// enum foo {
///     A,
///     B,
/// }
/// MyEnum::A;
///
/// // Fn
/// #[rename(name = "my-fn")]
/// fn foo(_: i32) {}
/// my_fn(1);
///
/// // Mod
/// #[rename(name = "my-mod")]
/// mod foo {
///     pub const A: i32 = 1;
/// }
/// assert_eq!(my_mod::A, 1);
///
/// // Static
/// #[rename(name = "my-static")]
/// static foo: i32 = 1;
/// assert_eq!(MY_STATIC, 1);
///
/// // Struct
/// #[rename(name = "my-struct")]
/// struct foo {
///     a: i32,
/// }
/// MyStruct { a: 1 };
///
/// // Trait
/// #[rename(name = "my-trait")]
/// trait foo {}
/// impl MyTrait for i32 {}
///
/// // Type
/// #[rename(name = "my-type")]
/// type foo = i32;
/// let _: MyType = 1;
///
/// // Union
/// #[rename(name = "my-union")]
/// union foo {
///     a: i32,
/// }
/// MyUnion { a: 1 };
/// ```
#[cfg(doctest)]
struct AdditionalTests;

#[cfg(test)]
mod tests {
    use crate::*;

    /// Tests some simple case conversions for all available case styles
    #[test]
    fn simple_case_conversion() {
        let tests = [
            (CaseStyle::LowerCamel, "_-fooBarBaz-_"),
            (CaseStyle::UpperCamel, "_-FooBarBaz-_"),
            (CaseStyle::Snake, "_-foo_bar_baz-_"),
            (CaseStyle::ShoutySnake, "_-FOO_BAR_BAZ-_"),
        ];

        for test in tests {
            let name = MacroInput {
                case:   Some(test.0),
                name:   Words(vec!["foo-bar".into(), "Baz".into()]),
                prefix: "_-".into(),
                suffix: "-_".into(),
            }
            .into_name(None)
            .unwrap();

            assert_eq!(name, test.1);
        }
    }
}
