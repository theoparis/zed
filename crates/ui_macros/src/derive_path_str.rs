use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Attribute, Data, DeriveInput, Expr, ExprLit, Lit};

pub fn derive_path_str(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let prefix = get_attr_value(&input.attrs, "prefix").expect("prefix attribute is required");
    let suffix = get_attr_value(&input.attrs, "suffix").unwrap_or_else(|| "".to_string());

    let serialize_all = get_strum_serialize_all(&input.attrs);
    let path_str_impl = impl_path_str(name, &input.data, &prefix, &suffix, serialize_all);

    let expanded = quote! {
        impl #name {
            pub fn path(&self) -> &'static str {
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
    serialize_all: Option<String>,
) -> proc_macro2::TokenStream {
    match *data {
        Data::Enum(ref data) => {
            let match_arms = data.variants.iter().map(|variant| {
                let ident = &variant.ident;
                let variant_name = if let Some(ref case) = serialize_all {
                    match case.as_str() {
                        "snake_case" => ident.to_string().to_case(Case::Snake),
                        "lowercase" => ident.to_string().to_lowercase(),
                        _ => ident.to_string(),
                    }
                } else {
                    ident.to_string()
                };
                let path = format!("{}/{}{}", prefix, variant_name, suffix);
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

fn get_strum_serialize_all(attrs: &[Attribute]) -> Option<String> {
    let mut found_value = None;

    for attr in attrs.iter().filter(|attr| attr.path().is_ident("strum")) {
        let _ = attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("serialize_all") {
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

fn get_attr_value(attrs: &[Attribute], key: &str) -> Option<String> {
    let mut found_value = None;

    for attr in attrs.iter().filter(|attr| attr.path().is_ident("path_str")) {
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
