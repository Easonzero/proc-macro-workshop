use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, Data, DeriveInput, Field, Fields};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = format_ident!("{}Builder", name);
    let members = foreach_members(&input.data, |f| {
        let name = format_ident!("builder_member_{}", f.ident.as_ref().unwrap());
        let ty = &f.ty;
        quote_spanned!(f.span()=> #name : Option<#ty>,)
    });
    let setters = foreach_members(&input.data, |f| {
        let name = &f.ident;
        let member_name = format_ident!("builder_member_{}", f.ident.as_ref().unwrap());
        let ty = &f.ty;
        quote_spanned! {f.span()=>
            pub fn #name (&mut self, v: #ty) -> &mut Self {
                self.#member_name = Some(v);
                self
            }
        }
    });
    let builder = foreach_members(&input.data, |f| {
        let name = &f.ident;
        let member_name = format_ident!("builder_member_{}", f.ident.as_ref().unwrap());
        quote_spanned! (f.span()=> #name : self.#member_name.clone().unwrap_or(Default::default()),)
    });
    let expanded = quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                Default::default()
            }
        }
        #[derive(Default)]
        pub struct #builder_name {
            #members
        }
        impl #builder_name {
            #setters
            pub fn build(&self) -> Result<#name, &'static str> {
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
