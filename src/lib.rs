use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, Token, Type,
};

struct Request {
    request_type: Ident,
    response_type: Type,
}

struct DeriveList {
    derives: Vec<Ident>,
}

struct RequestList {
    derives: Option<DeriveList>,
    requests: Vec<Request>,
}

impl Parse for DeriveList {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        syn::parenthesized!(content in input);

        let mut derives = Vec::new();
        while !content.is_empty() {
            derives.push(content.parse()?);
            if !content.is_empty() {
                content.parse::<Token![,]>()?;
            }
        }

        Ok(DeriveList { derives })
    }
}

impl Parse for RequestList {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let derives = if input.peek(syn::token::Paren) {
            let derives: DeriveList = input.parse()?;
            Some(derives)
        } else {
            None
        };

        let mut requests = Vec::new();

        while !input.is_empty() {
            requests.push(input.parse()?);
            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }

        Ok(RequestList { derives, requests })
    }
}

impl Parse for Request {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let request_type = input.parse()?;
        input.parse::<Token![=>]>()?;

        // Parse the entire response type as a single Type
        let response_type = input.parse()?;

        Ok(Request {
            request_type,
            response_type,
        })
    }
}

#[proc_macro]
pub fn define_requests(input: TokenStream) -> TokenStream {
    let RequestList { derives, requests } = parse_macro_input!(input as RequestList);

    // Create the derive attribute if specified
    let derive_attr = derives.map(|d| {
        let derives = d.derives;
        quote! { #[derive(#(#derives),*)] }
    });

    let responds_with_impls = requests.iter().map(|req| {
        let name = &req.request_type;
        let response_type = &req.response_type;

        quote! {
            impl RespondsWith<#response_type> for #name {
                fn to_enum(self) -> Requests {
                    Requests::#name(self)
                }

                fn resp_enum(r: #response_type) -> Responses {
                    Responses::#response_type(r)
                }

                fn resp_from_enum(r: Responses) -> #response_type {
                    match r {
                        Responses::#response_type(x) => x,
                        _ => panic!("broken code"),
                    }
                }
            }
        }
    });

    let request_enum_variants = requests.iter().map(|req| {
        let name = &req.request_type;
        quote! {
            #name(#name)
        }
    });

    let response_types: Vec<_> =
        requests
            .iter()
            .map(|req| &req.response_type)
            .fold(Vec::new(), |mut acc, ty| {
                if !acc
                    .iter()
                    .any(|x: &&Type| quote!(#x).to_string() == quote!(#ty).to_string())
                {
                    acc.push(ty);
                }
                acc
            });

    let expanded = quote! {
        #(#responds_with_impls)*

        pub trait RespondsWith<Resp> {
            fn to_enum(self) -> Requests;
            fn resp_enum(r: Resp) -> Responses;
            fn resp_from_enum(r: Responses) -> Resp;
        }

        #derive_attr
        #[derive(Debug)]
        pub enum Requests {
            #(#request_enum_variants,)*
        }

        impl Requests {
            pub fn name(&self) -> &'static str {
                match self {
                    #(Requests::#request_enum_variants => stringify!(#request_enum_variants),)*
                }
            }
        }

        #derive_attr
        #[derive(Debug)]
        pub enum Responses {
            #(#response_types(#response_types),)*
        }
    };

    expanded.into()
}
