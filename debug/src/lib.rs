use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, parse_quote, DeriveInput};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);
    input.generics = add_trait_bounds(input.generics);
    let name = &input.ident;
    let (imp_gen, ty_gen, where_gen) = input.generics.split_for_impl();

    let (members_key, members_value) = stringify_members(&input.data);

    let extends = quote!(
        impl#imp_gen std::fmt::Debug for #name#ty_gen
        #where_gen {
            fn fmt(&self, f:&mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, concat!(stringify!(#name), #members_key), #members_value)
            }
        }
    );

    proc_macro::TokenStream::from(extends)
}

fn add_trait_bounds(mut generics: syn::Generics) -> syn::Generics {
    for param in &mut generics.params {
        if let syn::GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    generics
}

fn format_of(attrs: &Vec<syn::Attribute>) -> TokenStream {
    fn mkerr<T>(token: T) -> TokenStream
    where
        T: quote::ToTokens,
    {
        syn::Error::new_spanned(token, "expect \"#[debug = ...]\"").to_compile_error()
    }
    for attr in attrs {
        if attr
            .path
            .segments
            .first()
            .map(|p| p.ident == "debug")
            .unwrap_or(false)
        {
            return match attr.parse_meta() {
                Ok(syn::Meta::NameValue(ref nvs)) => {
                    if let syn::Lit::Str(ref format_str) = nvs.lit {
                        quote!(#format_str)
                    } else {
                        mkerr(&nvs.lit)
                    }
                }
                Ok(_) => mkerr(attr),
                Err(e) => e.to_compile_error(),
            };
        }
    }
    quote!("{:#?}")
}

fn stringify_members(data: &syn::Data) -> (TokenStream, TokenStream) {
    match data {
        syn::Data::Struct(ref data) => match &data.fields {
            syn::Fields::Named(ref fields) => {
                let (key, value): (Vec<_>, Vec<_>) = fields
                    .named
                    .iter()
                    .filter(|f| {
                        if let syn::Type::Path(ref path) = &f.ty {
                            path.path.segments.iter()
                                .fold(true, |l, r| l && r.ident != "PhantomData")
                        } else {
                            true
                        }
                    })
                    .enumerate()
                    .map(|(i, f)| {
                        let name = f.ident.as_ref().unwrap();
                        let comma = if i == 0 { quote!(' ') } else { quote!(", ") };
                        let format = format_of(&f.attrs);
                        (
                            quote_spanned!(f.span()=>concat!(#comma, stringify!(#name), ": ", #format)),
                            quote_spanned!(f.span()=>self.#name),
                        )
                    })
                    .unzip();
                (
                    quote_spanned!(fields.span()=>" {{", #(#key),*, " }}"),
                    quote_spanned!(fields.span()=>#(#value),*),
                )
            }
            syn::Fields::Unnamed(_fields) => (quote!("{}"), quote!("")),
            syn::Fields::Unit => (quote!("{}"), quote!("")),
        },
        syn::Data::Enum(_data) => (quote!("{}"), quote!("")),
        syn::Data::Union(_data) => (quote!("{}"), quote!("")),
    }
}
