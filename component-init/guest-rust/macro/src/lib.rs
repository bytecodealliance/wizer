use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{ItemFn, parse_macro_input, spanned::Spanned};

#[proc_macro_attribute]
pub fn init(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);
    if input.sig.asyncness.is_some() {
        return quote_spanned! { input.sig.fn_token.span()=>
            compile_error!("fn cannot be async");
        }
        .into();
    }
    if !input.sig.inputs.is_empty() {
        return quote_spanned! { input.sig.inputs.span()=>
            compile_error!("function cannot have arguments");
        }
        .into();
    }
    let callee = &input.sig.ident;

    quote! {
        #input

        mod __component_init {
            component_init::__bindgen::generate!({
                inline: r"
                package this:wit;
                world w {
                    export component-init: func();
                }
                ",
                runtime_path: "component_init::__bindgen::rt",
            });
            struct __S;
            impl Guest for __S {
                fn component_init() {
                    super::#callee()
                }
            }
            export!(__S);
        }
    }
    .into()
}
