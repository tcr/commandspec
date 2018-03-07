extern crate shlex;
#[macro_use]
extern crate failure;

use failure::Error;
use std::process::Command;
use std::fmt;
use std::collections::HashMap;

pub trait CommandSpec {
    fn execute(&mut self) -> Result<(), CommandError>;
}

#[derive(Debug, Fail)]
pub enum CommandError {
    #[fail(display = "Encountered an IO error.")]
    Io(#[cause] ::std::io::Error),

    #[fail(display = "Command was interrupted.")]
    Interrupt,

    #[fail(display = "Command failed with error code {}.", _0)]
    Code(i32),
}

impl CommandSpec for Command {
    // Executes the command, and returns a versatile error struct
    fn execute(&mut self) -> Result<(), CommandError> {
        match self.status() {
            Ok(status) => {
                if status.success() {
                    Ok(())
                } else if let Some(code) = status.code() {
                    Err(CommandError::Code(code))
                } else {
                    Err(CommandError::Interrupt)
                }
            },
            Err(err) => Err(CommandError::Io(err)),
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

impl<'a> From<&'a u64> for CommandArg {
    fn from(value: &u64) -> Self {
        CommandArg::Literal(value.to_string())
    }
}

impl<'a> From<&'a f64> for CommandArg {
    fn from(value: &f64) -> Self {
        CommandArg::Literal(value.to_string())
    }
}

impl<'a> From<&'a i32> for CommandArg {
    fn from(value: &i32) -> Self {
        CommandArg::Literal(value.to_string())
    }
}

impl<'a> From<&'a i64> for CommandArg {
    fn from(value: &i64) -> Self {
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

pub fn commandify(value: String) -> Result<Command, Error> {
    let lines = value.trim().split("\n").map(String::from).collect::<Vec<_>>();

    #[derive(PartialEq)]
    enum SpecState {
        Cd,
        Env,
        Terminated,
    }

    let mut state = SpecState::Cd;

    let mut env = HashMap::<String, String>::new();
    let mut cd = None;
    let mut command = None;
    for raw_line in lines {
        let mut line = shlex::split(&raw_line).unwrap_or(vec![]);
        match line.get(0).map(|x| x.as_ref()) {
            None => continue,
            Some("cd") => {
                if state != SpecState::Cd {
                    bail!("cd should be the first line in your command! macro.");
                }
                ensure!(line.len() == 2, "Too many arguments in cd; expected 1, found {}", line.len() - 1);
                cd = Some(line.remove(1));
                state = SpecState::Env;
            }
            Some("export") => {
                if state != SpecState::Cd && state != SpecState::Env {
                    bail!("exports should follow cd but precede your command in the command! macro.");
                }
                ensure!(line.len() >= 2, "Not enough arguments in export; expected at least 1, found {}", line.len() - 1);
                for item in &line[1..] {
                    let mut items = item.splitn(2, "=").collect::<Vec<_>>();
                    ensure!(items.len() > 0, "Expected export of the format NAME=VALUE");
                    env.insert(items[0].to_string(), items[1].to_string());
                }
                state = SpecState::Env;
            }
            Some(_) => {
                if state != SpecState::Cd && state != SpecState::Env {
                    bail!("A command should be the last line in the command! macro.");
                }
                command = Some(line);
                state = SpecState::Terminated;
            }
        } 
    }
    if state != SpecState::Terminated {
        bail!("Didn't find a command in your command! macro.");
    }

    let mut command_args = command.unwrap();
    let mut cmd = Command::new(command_args.remove(0));
    cmd.args(command_args);
    for (key, value) in env {
        cmd.env(key, value);
    }
    if let Some(cd) = cd {
        cmd.current_dir(cd);
    }

    Ok(cmd)
}

#[macro_export]
macro_rules! commandspec {
    ($fmt:expr ,*) => ({
        $crate::commandify(format!($fmt))
    });
    ($fmt:expr, $( $id:ident = $value:expr ,)*) => ({
        $crate::commandify(
            format!($fmt, $( $id = $crate::command_arg(&$value) ,)*)
        )
    });
}
