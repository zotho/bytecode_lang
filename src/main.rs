use std::{
    collections::{HashMap, VecDeque},
    path::Path,
    str::FromStr,
};

type Value = u8;

#[derive(Debug)]
enum Instruction {
    LoadVal(Value),
    WriteVar(String),
    ReadVar(String),
    IfJump(String),
    Add,
    Sub,
    Mul,
    Div,
    ReturnValue,
    Nop,
    LoadAddress,
    Eq,
    NotEq,
    Jump,
    Lte,
    Lt,
    Gte,
    Gt,
    SendChannel,
    RecvChannel,
    Spawn,
}

const OPEN_QUOTE: char = '‘';
const CLOSE_QUOTE: char = '’';
const COMMENT: char = '#';

fn strip_quotes(s: &str) -> Option<&str> {
    s.strip_prefix(OPEN_QUOTE)
        .and_then(|v| v.strip_suffix(CLOSE_QUOTE))
}

impl FromStr for Instruction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once(" ") {
            Some((command, value)) => match command {
                "LOAD_VAL" => value.parse().map(Instruction::LoadVal).ok(),
                "WRITE_VAR" => strip_quotes(value).map(|v| Instruction::WriteVar(v.to_string())),
                "READ_VAR" => strip_quotes(value).map(|v| Instruction::ReadVar(v.to_string())),
                "IF_JUMP" => strip_quotes(value).map(|v| Instruction::IfJump(v.to_string())),
                _ => None,
            },
            None => match s {
                "ADD" => Some(Instruction::Add),
                "SUB" => Some(Instruction::Sub),
                "MUL" => Some(Instruction::Mul),
                "DIV" => Some(Instruction::Div),
                "RETURN_VALUE" => Some(Instruction::ReturnValue),
                "NOP" => Some(Instruction::Nop),
                "LOAD_ADDRESS" => Some(Instruction::LoadAddress),
                "EQ" => Some(Instruction::Eq),
                "NOT_EQ" => Some(Instruction::NotEq),
                "GT" => Some(Instruction::Gt),
                "GTE" => Some(Instruction::Gte),
                "LT" => Some(Instruction::Lt),
                "LTE" => Some(Instruction::Lte),
                "JUMP" => Some(Instruction::Jump),
                "SEND_CHANNEL" => Some(Instruction::SendChannel),
                "RECV_CHANNEL" => Some(Instruction::RecvChannel),
                "SPAWN" => Some(Instruction::Spawn),
                _ => None,
            },
        }
        .ok_or_else(|| s.to_string())
    }
}

#[derive(Debug)]
struct ByteCode {
    pub instructions: Vec<Instruction>,
}

fn strip_comment(line: &str) -> &str {
    let mut quotes: usize = 0;
    for (i, ch) in line.char_indices() {
        match ch {
            OPEN_QUOTE => quotes = quotes.checked_add(1).unwrap(),
            CLOSE_QUOTE => quotes = quotes.checked_sub(1).unwrap(),
            COMMENT => {
                if quotes == 0 {
                    return line.get(..i).unwrap().trim_end();
                }
            }
            _ => (),
        }
    }
    line
}

impl ByteCode {
    fn load<P: AsRef<Path>>(path: P) -> Result<Self, String> {
        let code = std::fs::read_to_string(path).unwrap();
        let res: Result<Vec<_>, _> = code
            .lines()
            .map(strip_comment)
            .map(|line| if line.is_empty() { "NOP" } else { line })
            .map(|line| line.parse())
            .collect();
        res.map(|instructions| Self { instructions })
    }
}

#[derive(Debug, Default)]
struct CoroState {
    address: usize,
    stack: Vec<Value>,
    exited: bool,
}

#[derive(Debug, Default)]
struct Interpreter {
    coroutines: Vec<CoroState>,
    scope: HashMap<String, Value>,
    channels: HashMap<Value, VecDeque<Value>>,
    steps: usize,
}

fn apply_binary(stack: &mut Vec<Value>, op: impl FnOnce(Value, Value) -> Value) {
    let b = stack.pop().unwrap();
    let a = stack.pop().unwrap();
    stack.push(op(a, b));
}

impl Interpreter {
    fn interpret(&mut self, bytecode: &ByteCode) -> Value {
        self.coroutines.push(CoroState {
            address: 0,
            stack: Vec::new(),
            exited: false,
        });

        'main_loop: loop {
            let mut created_coros = Vec::new();
            for (
                i_coro,
                CoroState {
                    address,
                    stack,
                    exited,
                },
            ) in self.coroutines.iter_mut().enumerate()
            {
                let mut after_jump = false;
                let instruction = &bytecode.instructions[*address];
                eprintln!("{}{instruction:?}", "\t\t".repeat(i_coro));
                match instruction {
                    Instruction::LoadVal(value) => stack.push(*value),
                    Instruction::WriteVar(variable) => {
                        self.scope.insert(variable.clone(), stack.pop().unwrap());
                    }
                    Instruction::ReadVar(variable) => {
                        stack.push(*self.scope.get(variable).unwrap());
                    }
                    Instruction::Add => apply_binary(stack, |a, b| a.checked_add(b).unwrap()),
                    Instruction::Sub => apply_binary(stack, |a, b| a.checked_sub(b).unwrap()),
                    Instruction::Mul => apply_binary(stack, |a, b| a.checked_mul(b).unwrap()),
                    Instruction::Div => apply_binary(stack, |a, b| a.checked_div(b).unwrap()),
                    Instruction::ReturnValue => {
                        assert_eq!(stack.len(), 1);
                        let result = stack[0];
                        if i_coro == 0 {
                            break 'main_loop result;
                        } else {
                            assert_eq!(result, 0, "Coro ended with non-zero status");
                            *exited = true;
                        }
                    }
                    Instruction::Nop => (),
                    Instruction::LoadAddress => {
                        stack.push((*address).try_into().unwrap());
                    }
                    Instruction::Eq => apply_binary(stack, |a, b| (a == b).into()),
                    Instruction::NotEq => apply_binary(stack, |a, b| (a != b).into()),
                    Instruction::Lte => apply_binary(stack, |a, b| (a <= b).into()),
                    Instruction::Lt => apply_binary(stack, |a, b| (a < b).into()),
                    Instruction::Gte => apply_binary(stack, |a, b| (a >= b).into()),
                    Instruction::Gt => apply_binary(stack, |a, b| (a > b).into()),
                    Instruction::Jump => {
                        *address = stack.pop().unwrap() as usize;
                        after_jump = true;
                    }
                    Instruction::IfJump(variable) => {
                        let condition = stack.pop().unwrap() != 0;
                        if condition {
                            *address = *self.scope.get(variable).unwrap() as usize;
                            after_jump = true;
                        }
                    }
                    Instruction::SendChannel => {
                        let x = stack.pop().unwrap();
                        let channel = stack.pop().unwrap();
                        self.channels.entry(channel).or_default().push_front(x);
                    }
                    Instruction::RecvChannel => {
                        let channel = stack.pop().unwrap();
                        let entry = self.channels.get_mut(&channel).unwrap();
                        let x = entry.pop_back().unwrap();
                        stack.push(x);
                    }
                    Instruction::Spawn => {
                        let b = stack.pop().unwrap() as usize;
                        let a = stack.pop().unwrap() as usize;
                        *address = a;
                        after_jump = true;
                        created_coros.push(CoroState {
                            address: b,
                            stack: Vec::new(),
                            exited: false,
                        });
                    }
                }
                if !after_jump {
                    *address += 1;
                }
                self.steps += 1;
                if self.steps >= 1000 {
                    panic!("Got stuck: {self:?}");
                }
            }
            self.coroutines.retain(|coro| !coro.exited);
            self.coroutines.extend(created_coros);
        }
    }
}

fn main() {
    let bytecode = ByteCode::load("code/test_1.code").unwrap();
    let mut interpreter = Interpreter::default();
    let result = interpreter.interpret(&bytecode);
    println!();
    println!("Result: {result}");
    assert_eq!(result, 4);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interpret_bytecode() {
        let bytecode = ByteCode::load("code/test_1.code").unwrap();
        let mut interpreter = Interpreter::default();
        let result = interpreter.interpret(&bytecode);
        assert_eq!(result, 4);
    }

    #[test]
    #[should_panic]
    fn test_interpret_bytecode_panic() {
        let bytecode = ByteCode::load("code/test_2.code").unwrap();
        let mut interpreter = Interpreter::default();
        let result = interpreter.interpret(&bytecode);
        assert_eq!(result, 4);
    }

    #[test]
    fn test_interpret_bytecode_2() {
        let bytecode = ByteCode::load("code/test_3.code").unwrap();
        let mut interpreter = Interpreter::default();
        let result = interpreter.interpret(&bytecode);
        assert_eq!(result, 3);
    }

    #[test]
    fn test_interpret_bytecode_loop() {
        let bytecode = ByteCode::load("code/test_4.code").unwrap();
        let mut interpreter = Interpreter::default();
        let result = interpreter.interpret(&bytecode);
        assert_eq!(result, 10);
    }

    #[test]
    fn test_interpret_bytecode_fib() {
        // 1 1 2 3 5 8 13 21 34
        fn rust_fib(n: Value) -> Value {
            let n = n;
            let mut i = 0;
            let mut x = 1;
            let mut y = 1;
            loop {
                (x, y) = (y, x + y);
                i += 1;
                if i < n {
                    continue;
                }
                break x;
            }
        }
        assert_eq!(rust_fib(8), 34);

        let bytecode = ByteCode::load("code/test_5_fib.code").unwrap();
        let mut interpreter = Interpreter::default();
        let result = interpreter.interpret(&bytecode);
        assert_eq!(result, 34);
    }

    #[test]
    fn test_interpret_bytecode_send_recv_channel() {
        let bytecode = ByteCode::load("code/test_6.code").unwrap();
        let mut interpreter = Interpreter::default();
        let result = interpreter.interpret(&bytecode);
        assert_eq!(result, 3);
    }
}
