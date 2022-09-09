use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let expanded = quote! {
        pub struct CommandBuilder {
            executable: Option<String>,
            args: Option<Vec<String>>,
            env: Option<Vec<String>>,
            current_dir: Option<String>,
        }

        impl Command {
            pub fn builder() -> CommandBuilder {
                CommandBuilder {
                    executable: None,
                    args: None,
                    env: None,
                    current_dir: None,
                }
            }
        }

        impl CommandBuilder {
            fn executable(&mut self, executable: String) -> &mut Self {
                self.executable = Some(executable);
                self
            }

            fn args(&mut self, args: Vec<String>) -> &mut Self {
                self.args = Some(args);
                self
            }
            fn env(&mut self, env: Vec<String>) -> &mut Self {
                self.env = Some(env);
                self
            }
            fn current_dir(&mut self, current_dir: String) -> &mut Self {
                self.current_dir = Some(current_dir);
                self
            }

            pub fn build(&mut self) -> Result<Command, Box<dyn std::error::Error>> {
                if self.executable.is_none() || self.args.is_none() || self.env.is_none() || self.current_dir.is_none() {
                    Err("Error occured!".into())
                } else {
                    Ok(Command {
                        executable: self.executable.clone().unwrap(),
                        args: self.args.clone().unwrap(),
                        env: self.env.clone().unwrap(),
                        current_dir: self.current_dir.clone().unwrap(),
                    })
                }
            }
        };
    };

    TokenStream::from(expanded)
}
