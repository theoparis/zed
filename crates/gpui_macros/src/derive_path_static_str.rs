use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Attribute, Data, DeriveInput, Expr, ExprLit, Lit};

pub fn derive_path_static_str(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let prefix = get_attr_value(&input.attrs, "prefix").unwrap_or_else(|| "".to_string());
    let suffix = get_attr_value(&input.attrs, "suffix").unwrap_or_else(|| "".to_string());
    let delimiter = get_attr_value(&input.attrs, "delimiter").unwrap_or_else(|| "/".to_string());

    let path_str_impl = impl_path_str(name, &input.data, &prefix, &suffix, &delimiter);

    let expanded = quote! {
        impl #name {
            pub fn path_str(&self) -> &'static str {
                #path_str_impl
            }
        }
    };

    TokenStream::from(expanded)
}

fn impl_path_str(
    name: &syn::Ident,
    data: &Data,
    prefix: &str,
    suffix: &str,
    delimiter: &str,
) -> proc_macro2::TokenStream {
    match *data {
        Data::Enum(ref data) => {
            let match_arms = data.variants.iter().map(|variant| {
                let ident = &variant.ident;
                let path = format!("{}{}{}{}{}", prefix, delimiter, ident, delimiter, suffix);
                quote! {
                    #name::#ident => #path,
                }
            });

            quote! {
                match self {
                    #(#match_arms)*
                }
            }
        }
        _ => panic!("DerivePathStr only supports enums"),
    }
}

fn get_attr_value(attrs: &[Attribute], key: &str) -> Option<String> {
    let mut found_value = None;

    for attr in attrs
        .iter()
        .filter(|attr| attr.path().is_ident("derive_path_static_str"))
    {
        let _ = attr.parse_nested_meta(|meta| {
            if meta.path.is_ident(key) {
                if let Expr::Lit(ExprLit {
                    lit: Lit::Str(lit_str),
                    ..
                }) = meta.value()?.parse()?
                {
                    found_value = Some(lit_str.value());
                }
            }
            Ok(())
        });

        if found_value.is_some() {
            break;
        }
    }

    found_value
}
