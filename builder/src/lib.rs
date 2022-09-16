use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, Data, DeriveInput, Fields, FieldsNamed, GenericArgument, Path,
    PathArguments, PathSegment, Type, TypePath,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;
    let builder_name = format_ident!("{}Builder", struct_name);
    let fields = extract_struct_fields(&input.data);

    let wrapped_fields_stream_iter = fields.named.iter().map(|field| {
        let ty = &field.ty;
        let ident = &field.ident;
        if is_option_type(&ty) {
            quote! {
                #ident: #ty
            }
        } else {
            quote! {
                #ident: Option<#ty>
            }
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
        if is_option_type(&ty) {
            let inner_type = option_inner_type(&ty);
            quote! {
                fn #ident(&mut self, #ident: #inner_type) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        } else {
            quote! {
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = Some(#ident);
                    self
                }
            }
        }
    });
    let builder_build_stream_iter = fields.named.iter().map(|field| {
        let ty = &field.ty;
        let ident = &field.ident;
        if is_option_type(&ty) {
            quote! {
                #ident: self.#ident.clone()
            }
        } else {
            quote! {
                #ident: self.#ident.clone().unwrap()
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

            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                Ok(#struct_name {
                    #(#builder_build_stream_iter),*
                })
            }
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

fn is_option_type(ty: &Type) -> bool {
    match last_path_segment(&ty) {
        Some(path_seg) => path_seg.ident == "Option",
        None => false,
    }
}

fn option_inner_type(ty: &Type) -> &GenericArgument {
    match last_path_segment(&ty) {
        Some(PathSegment {
            ident: _,
            arguments: PathArguments::AngleBracketed(ref gen_arg),
        }) => gen_arg.args.first(),
        _ => None,
    }
    .expect("invalid option type")
}

fn last_path_segment(ty: &Type) -> Option<&PathSegment> {
    match ty {
        &Type::Path(TypePath {
            qself: None,
            path:
                Path {
                    segments: ref seg,
                    leading_colon: _,
                },
        }) => seg.last(),
        _ => None,
    }
}
