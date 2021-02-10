use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, Data, DeriveInput, Field, Fields, Type};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = format_ident!("{}Builder", name);
    let members = foreach_members(&input.data, |f| {
        let name = format_ident!("builder_member_{}", f.ident.as_ref().unwrap());
        let ty = &f.ty;
        quote_spanned!(f.span()=> #name : std::option::Option<#ty>,)
    });
    let setters = foreach_members(&input.data, |f| {
        let name = &f.ident;
        let member_name = format_ident!("builder_member_{}", f.ident.as_ref().unwrap());
        if let Some(inner_ty) = ty_inner_type("Option", &f.ty) {
            let ty = inner_ty;
            quote_spanned! {f.span()=>
                pub fn #name (&mut self, v: #ty) -> &mut Self {
                    self.#member_name = std::option::Option::Some(std::option::Option::Some(v));
                    self
                }
            }
        } else {
            let ty = &f.ty;
            let (repeat, expanded) = if let Some(attr) = f.attrs.first() {
                if attr.path.segments[0].ident == "builder" {
                    let each = if let Ok(syn::Meta::List(ref nvs)) = attr.parse_meta() {
                        if nvs.path.get_ident().unwrap() != "builder" {
                            return syn::Error::new_spanned(
                                nvs,
                                "expected `builder(each = \"...\")`",
                            )
                            .to_compile_error();
                        }
                        if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(kv))) =
                            nvs.nested.first()
                        {
                            if kv.path.get_ident().unwrap() != "each" {
                                return syn::Error::new_spanned(
                                    nvs,
                                    "expected `builder(each = \"...\")`",
                                )
                                .to_compile_error();
                            }
                            match &kv.lit {
                                syn::Lit::Str(each) => {
                                    syn::Ident::new(each.value().as_str(), each.span())
                                }
                                _ => unreachable!(),
                            }
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!()
                    };
                    let inner_ty = ty_inner_type("Vec", ty).unwrap();
                    (
                        name.as_ref().unwrap() == &each,
                        quote_spanned! {f.span()=>
                            pub fn #each (&mut self, #each: #inner_ty) -> &mut Self {
                                if let std::option::Option::Some(ref mut container) = self.#member_name {
                                    container.push(#each);
                                } else {
                                    self.#member_name = std::option::Option::Some(vec![#each]);
                                }
                                self
                            }
                        },
                    )
                } else {
                    (false, quote!())
                }
            } else {
                (false, quote!())
            };
            if repeat {
                quote_spanned!(f.span()=> #expanded)
            } else {
                quote_spanned! {f.span()=>
                    pub fn #name (&mut self, v: #ty) -> &mut Self {
                        self.#member_name = std::option::Option::Some(v);
                        self
                    }
                    #expanded
                }
            }
        }
    });
    let builder = foreach_members(&input.data, |f| {
        let name = &f.ident;
        let member_name = format_ident!("builder_member_{}", f.ident.as_ref().unwrap());
        quote_spanned!(f.span()=> #name : self.#member_name.clone().unwrap_or(std::default::Default::default()),)
    });
    let expanded = quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                std::default::Default::default()
            }
        }
        #[derive(std::default::Default)]
        pub struct #builder_name {
            #members
        }
        impl #builder_name {
            #setters
            pub fn build(&self) -> std::result::Result<#name, &'static str> {
                Ok(#name {
                    #builder
                })
            }
        }
    };
    proc_macro::TokenStream::from(expanded)
}

fn foreach_members(data: &Data, f: impl Fn(&Field) -> TokenStream) -> TokenStream {
    match data {
        Data::Struct(ref item) => match item.fields {
            Fields::Named(ref fields) => {
                let recurse = fields.named.iter().map(f);
                quote!(#(#recurse)*)
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn ty_inner_type<'a>(wrapper: &str, ty: &'a Type) -> Option<&'a Type> {
    if let Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper {
            return None;
        }

        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }

            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}
