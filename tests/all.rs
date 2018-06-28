#[macro_use]
extern crate commandspec;

mod sh {
    #[test]
    fn sh_exit() {
        let res = sh_execute!(r"exit {a}", a = 42).unwrap_err();
        assert_eq!(res.error_code(), 42);
    }

    #[test]
    fn sh_echo() {
        let res = sh_command!(r"A={a}; echo $A", a = "SENTINEL").unwrap().output().unwrap();
        assert_eq!(res.stdout, b"SENTINEL\n");
    }

    #[test]
    fn sh_empty() {
        sh_execute!(r"true").unwrap();
    }

    #[test]
    fn sh_empty_comma() {
        sh_execute!(r"true", ).unwrap();
    }
}
