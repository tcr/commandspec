extern crate shlex;

use std::process::Command;
use std::fmt;

pub trait ExecuteCommand {
    fn execute(&mut self);
}

impl ExecuteCommand for Command {
    fn execute(&mut self) {
        if !self.status().expect("Failed to run command.").success() {
            ::std::process::exit(1);
        }
    }
}

pub enum CommandArg {
    Empty,
    Literal(String),
    List(Vec<String>),
}

impl fmt::Display for CommandArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use CommandArg::*;
        match *self {
            Empty => write!(f, ""),
            Literal(ref value) => {
                write!(f, "{}", shlex::quote(&format!("{}", value)))
            },
            List(ref list) => {
                write!(f, "{}", list
                    .iter()
                    .map(|x| shlex::quote(&format!("{}", x)).to_string())
                    .collect::<Vec<_>>()
                    .join(" "))
            }
        }
    }
}

impl<'a, 'b> From<&'a &'b str> for CommandArg {
    fn from(value: &&str) -> Self {
        CommandArg::Literal(value.to_string())
    }
}

impl<'a> From<&'a str> for CommandArg {
    fn from(value: &str) -> Self {
        CommandArg::Literal(value.to_string())
    }
}

impl<'a, T> From<&'a [T]> for CommandArg
    where T: fmt::Display {
    fn from(list: &[T]) -> Self {
        CommandArg::List(
            list
                .iter()
                .map(|x| format!("{}", x))
                .collect()
        )
    }
}

impl<'a, T> From<&'a Vec<T>> for CommandArg
    where T: fmt::Display {
    fn from(list: &Vec<T>) -> Self {
        CommandArg::from(list.as_slice())
    }
}

impl<'a, T> From<&'a Option<T>> for CommandArg
    where T: fmt::Display {
    fn from(opt: &Option<T>) -> Self {
        if let Some(ref value) = *opt {
            CommandArg::Literal(format!("{}", value))
        } else {
            CommandArg::Empty
        }
    }
}

pub fn command_arg<'a, T>(value: &'a T) -> CommandArg
    where CommandArg: std::convert::From<&'a T> {
    CommandArg::from(value)
}

pub fn commandify(value: String) -> Command {
    let mut args = shlex::split(value.trim()).unwrap_or(vec![]);
    let cmd_text = args.remove(0);
    let mut cmd = Command::new(cmd_text);
    cmd.args(args);
    cmd
}

#[macro_export]
macro_rules! command {
    (cd: $cd:expr, $($args:tt)*) => ({
        command!($($args)*)
            .current_dir($cd)
    });
    (env: $name:ident = $value:expr, $($args:tt)*) => ({
        command!($($args)*)
            .env(stringify!($name), format!("{}", $value))
    });
    ($fmt:expr ,*) => ({
        ::command_macro::commandify(format!($fmt))
    });
    ($fmt:expr, $( $id:ident = $value:expr ,)*) => ({
        ::command_macro::commandify(
            format!($fmt, $( $id = ::command_macro::command_arg(&$value) ,)*)
        )
    });
}
