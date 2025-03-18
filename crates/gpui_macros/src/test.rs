use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{format_ident, quote};
use std::mem;
use syn::{
    parse_quote, spanned::Spanned, Error, Expr, ExprLit, FnArg, ItemFn, Lit, Meta, MetaList,
    MetaNameValue, PathSegment, Type,
};

pub fn test(args: TokenStream, function: TokenStream) -> TokenStream {
    let mut seeds = Vec::<u64>::new();
    let mut max_retries = 0;
    let mut num_iterations = 1;
    let mut on_failure_fn_name = quote!(None);

    let args_parser = syn::meta::parser(|arg| {
        arg.parse_nested_meta(|meta| {
            let ident = {
                let Some(ident) = meta.path.get_ident() else {
                    return Err(Error::new(meta.path.span(), "unexpected path"));
                };

                ident.to_string()
            };

            match (meta.value()?.parse()?, ident.as_str()) {
                (
                    Meta::NameValue(MetaNameValue {
                        value: Expr::Lit(ExprLit { lit, .. }),
                        ..
                    }),
                    "retries",
                ) => max_retries = parse_usize(&lit).unwrap(),
                (
                    Meta::NameValue(MetaNameValue {
                        value: Expr::Lit(ExprLit { lit, .. }),
                        ..
                    }),
                    "iterations",
                ) => num_iterations = parse_usize(&lit).unwrap(),
                (Meta::NameValue(meta), "on_failure") => {
                    let Expr::Lit(ExprLit {
                        lit: Lit::Str(name),
                        ..
                    }) = &meta.value
                    else {
                        return Err(Error::new(
                            meta.value.span(),
                            "on_failure argument must be a string",
                        ));
                    };
                    let segments = name
                        .value()
                        .split("::")
                        .map(|part| PathSegment::from(Ident::new(part, name.span())))
                        .collect();
                    let path = syn::Path {
                        leading_colon: None,
                        segments,
                    };
                    on_failure_fn_name = quote!(Some(#path));
                }
                (
                    Meta::NameValue(MetaNameValue {
                        value: Expr::Lit(ExprLit { lit, .. }),
                        ..
                    }),
                    "seed",
                ) => {
                    seeds = vec![parse_usize(&lit)? as u64];
                }
                (Meta::List(list), "seeds") => seeds = parse_u64_array(&list)?,
                (Meta::Path(path), _) => {
                    return Err(Error::new(path.span(), "invalid path argument"));
                }
                (value, _) => {
                    return Err(Error::new(value.span(), "invalid argument name"));
                }
            }

            Ok(())
        })
    });

    syn::parse_macro_input!(args with args_parser);
    let seeds = quote!( #(#seeds),* );

    let mut inner_fn = match syn::parse::<ItemFn>(function) {
        Ok(inner_fn) => inner_fn,
        Err(err) => {
            return error_to_stream(err);
        }
    };
    let inner_fn_attributes = mem::take(&mut inner_fn.attrs);
    let inner_fn_name = format_ident!("_{}", inner_fn.sig.ident);
    let outer_fn_name = mem::replace(&mut inner_fn.sig.ident, inner_fn_name.clone());

    let mut outer_fn: ItemFn = if inner_fn.sig.asyncness.is_some() {
        // Pass to the test function the number of app contexts that it needs,
        // based on its parameter list.
        let mut cx_vars = proc_macro2::TokenStream::new();
        let mut cx_teardowns = proc_macro2::TokenStream::new();
        let mut inner_fn_args = proc_macro2::TokenStream::new();
        for (ix, arg) in inner_fn.sig.inputs.iter().enumerate() {
            if let FnArg::Typed(arg) = arg {
                if let Type::Path(ty) = &*arg.ty {
                    let last_segment = ty.path.segments.last();
                    match last_segment.map(|s| s.ident.to_string()).as_deref() {
                        Some("StdRng") => {
                            inner_fn_args.extend(quote!(rand::SeedableRng::seed_from_u64(_seed),));
                            continue;
                        }
                        Some("BackgroundExecutor") => {
                            inner_fn_args.extend(quote!(gpui::BackgroundExecutor::new(
                                std::sync::Arc::new(dispatcher.clone()),
                            ),));
                            continue;
                        }
                        _ => {}
                    }
                } else if let Type::Reference(ty) = &*arg.ty {
                    if let Type::Path(ty) = &*ty.elem {
                        let last_segment = ty.path.segments.last();
                        if let Some("TestAppContext") =
                            last_segment.map(|s| s.ident.to_string()).as_deref()
                        {
                            let cx_varname = format_ident!("cx_{}", ix);
                            cx_vars.extend(quote!(
                                let mut #cx_varname = gpui::TestAppContext::new(
                                    dispatcher.clone(),
                                    Some(stringify!(#outer_fn_name)),
                                );
                            ));
                            cx_teardowns.extend(quote!(
                                dispatcher.run_until_parked();
                                #cx_varname.quit();
                                dispatcher.run_until_parked();
                            ));
                            inner_fn_args.extend(quote!(&mut #cx_varname,));
                            continue;
                        }
                    }
                }
            }

            return error_to_stream(Error::new(arg.span(), "invalid function signature"));
        }

        parse_quote! {
            #[test]
            fn #outer_fn_name() {
                #inner_fn

                gpui::run_test(
                    #num_iterations,
                    &[#seeds],
                    #max_retries,
                    &mut |dispatcher, _seed| {
                        let executor = gpui::BackgroundExecutor::new(std::sync::Arc::new(dispatcher.clone()));
                        #cx_vars
                        executor.block_test(#inner_fn_name(#inner_fn_args));
                        #cx_teardowns
                    },
                    #on_failure_fn_name
                );
            }
        }
    } else {
        // Pass to the test function the number of app contexts that it needs,
        // based on its parameter list.
        let mut cx_vars = proc_macro2::TokenStream::new();
        let mut cx_teardowns = proc_macro2::TokenStream::new();
        let mut inner_fn_args = proc_macro2::TokenStream::new();
        for (ix, arg) in inner_fn.sig.inputs.iter().enumerate() {
            if let FnArg::Typed(arg) = arg {
                if let Type::Path(ty) = &*arg.ty {
                    let last_segment = ty.path.segments.last();

                    if let Some("StdRng") = last_segment.map(|s| s.ident.to_string()).as_deref() {
                        inner_fn_args.extend(quote!(rand::SeedableRng::seed_from_u64(_seed),));
                        continue;
                    }
                } else if let Type::Reference(ty) = &*arg.ty {
                    if let Type::Path(ty) = &*ty.elem {
                        let last_segment = ty.path.segments.last();
                        match last_segment.map(|s| s.ident.to_string()).as_deref() {
                            Some("App") => {
                                let cx_varname = format_ident!("cx_{}", ix);
                                let cx_varname_lock = format_ident!("cx_{}_lock", ix);
                                cx_vars.extend(quote!(
                                    let mut #cx_varname = gpui::TestAppContext::new(
                                       dispatcher.clone(),
                                       Some(stringify!(#outer_fn_name))
                                    );
                                    let mut #cx_varname_lock = #cx_varname.app.borrow_mut();
                                ));
                                inner_fn_args.extend(quote!(&mut #cx_varname_lock,));
                                cx_teardowns.extend(quote!(
                                    drop(#cx_varname_lock);
                                    dispatcher.run_until_parked();
                                    #cx_varname.update(|cx| { cx.quit() });
                                    dispatcher.run_until_parked();
                                ));
                                continue;
                            }
                            Some("TestAppContext") => {
                                let cx_varname = format_ident!("cx_{}", ix);
                                cx_vars.extend(quote!(
                                    let mut #cx_varname = gpui::TestAppContext::new(
                                        dispatcher.clone(),
                                        Some(stringify!(#outer_fn_name))
                                    );
                                ));
                                cx_teardowns.extend(quote!(
                                    dispatcher.run_until_parked();
                                    #cx_varname.quit();
                                    dispatcher.run_until_parked();
                                ));
                                inner_fn_args.extend(quote!(&mut #cx_varname,));
                                continue;
                            }
                            _ => {}
                        }
                    }
                }
            }

            return error_to_stream(Error::new(arg.span(), "invalid function signature"));
        }

        parse_quote! {
            #[test]
            fn #outer_fn_name() {
                #inner_fn

                gpui::run_test(
                    #num_iterations,
                    &[#seeds],
                    #max_retries,
                    &mut |dispatcher, _seed| {
                        #cx_vars
                        #inner_fn_name(#inner_fn_args);
                        #cx_teardowns
                    },
                    #on_failure_fn_name,
                );
            }
        }
    };
    outer_fn.attrs.extend(inner_fn_attributes);

    TokenStream::from(quote!(#outer_fn))
}

fn parse_usize(literal: &Lit) -> Result<usize, Error> {
    let Lit::Int(int) = &literal else {
        return Err(Error::new(literal.span(), "expected an usize"));
    };

    int.base10_parse()
}

fn parse_u64_array(meta_list: &MetaList) -> Result<Vec<u64>, Error> {
    let mut array = vec![];

    meta_list.parse_nested_meta(|meta| {
        let meta = meta.value()?.parse()?;

        if let Meta::NameValue(MetaNameValue {
            value: Expr::Lit(ExprLit { lit, .. }),
            ..
        }) = meta
        {
            array.push(parse_usize(&lit).map(|value| value as u64)?);
            return Ok(());
        }

        Err(Error::new(meta.span(), "expected an integer"))
    })?;

    Ok(array)
}

fn error_to_stream(err: syn::Error) -> TokenStream {
    TokenStream::from(err.into_compile_error())
}
