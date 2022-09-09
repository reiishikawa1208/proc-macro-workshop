use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Fields, FieldsNamed};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;
    let builder_name = format_ident!("{}Builder", struct_name);
    let fields = extract_struct_fields(&input.data);

    let wrapped_fields_stream_iter = fields.named.iter().map(|field| {
        let ty = &field.ty;
        let ident = &field.ident;
        quote! {
            #ident: Option<#ty>
        }
    });
    let initial_fileds_stream_iter = fields.named.iter().map(|field| {
        let ident = &field.ident;
        quote! {
            #ident: None
        }
    });
    let builder_fields_setter_stream_iter = fields.named.iter().map(|field| {
        let ty = &field.ty;
        let ident = &field.ident;
        quote! {
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        }
    });

    let expanded = quote! {
        pub struct #builder_name {
            #(#wrapped_fields_stream_iter),*
        }

        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#initial_fileds_stream_iter),*
                }
            }
        }

        impl #builder_name {
            #(#builder_fields_setter_stream_iter)*
        }
    };

    proc_macro::TokenStream::from(expanded)
}

fn extract_struct_fields(data: &Data) -> &FieldsNamed {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => fields,
            _ => panic!("invalid fields"),
        },
        _ => panic!("invalid data"),
        // Data::Enum(_) => {}
        // Data::Union(_) => {}
    }
}
