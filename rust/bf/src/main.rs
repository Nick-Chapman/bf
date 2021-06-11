
use std::fs;
use std::io::Read;
use std::env;

fn main() {
    //println!("*rusty-bf*")

    let mut args = env::args();
    args.next();
    let filename = & args.next().expect("need filename");
    //println!("filename: {}", filename);

    let contents = fs::read_to_string(filename).expect(filename);
    //println!("contents: {}", contents);

    let prog = Prog::parse(&contents);
    //println!("prog: {:?}",prog);

    let mut m1 = Machine::of_prog(&prog);
    m1.run()
}

#[derive(Debug)]
enum Op {
    Dot,
    Comma,
    Plus,
    Minus,
    Langle,
    Rangle,
    Lsquare,
    Rsquare,
}

#[derive(Debug)]
struct Prog (Vec<Op>);

impl Prog {
    fn parse(s: &str) -> Prog {
        use Op::*;
        let mut ops : Vec<Op> = Vec::new();
        for c in s.chars() {
            match c {
                '.' => { ops.push(Dot) }
                ',' => { ops.push(Comma) }
                '+' => { ops.push(Plus) }
                '-' => { ops.push(Minus) }
                '<' => { ops.push(Langle) }
                '>' => { ops.push(Rangle) }
                '[' => { ops.push(Lsquare) }
                ']' => { ops.push(Rsquare) }
                _ => {}
            }
        }
        Prog(ops)
    }
}

impl Op {
    fn exec(&self, m: &mut Machine) {
        //println!("exec: op={:?}, m={:?}",self,m);
        use Op::*;
        match self {
            Dot => {
                m.mem.read(m.mp).putchar()
            }
            Comma => {
                m.mem.read(m.mp).getchar()
            }
            Plus => {
                m.mem.read(m.mp).increment()
            }
            Minus => {
                m.mem.read(m.mp).decrement()
            }
            Langle => {
                m.mp -= 1
            }
            Rangle => {
                m.mp += 1
            }
            Lsquare => {
                let Prog(code) = &m.prog;
                if m.mem.read(m.mp).zero() {
                    let mut nesting = 1;
                    loop {
                        if nesting == 0 { break }
                        m.ip += 1;
                        let op = &code[m.ip];
                        match op {
                            Op::Lsquare => { nesting += 1 }
                            Op::Rsquare => { nesting -= 1 }
                            _ => {}
                        }
                    }
                }
            }
            Rsquare => {
                let Prog(code) = &m.prog;
                if !m.mem.read(m.mp).zero() {
                    let mut nesting = 1;
                    loop {
                        if nesting == 0 { break }
                        m.ip -= 1;
                        let op = &code[m.ip];
                        match op {
                            Op::Lsquare => { nesting -= 1 }
                            Op::Rsquare => { nesting += 1 }
                            _ => {}
                        }
                    }
                }

            }
        }
    }
}

#[derive(Debug)]
struct Machine<'a> {
    prog: &'a Prog,
    mem: Mem,
    ip: usize,
    mp: usize,
}

const MEM_SIZE: usize = 1000;

impl Machine<'_> {

    fn of_prog<'a>(prog: &'a Prog) -> Machine<'a> {
        let mem = Mem(vec![Cell::new(0); MEM_SIZE]);
        Machine { prog, mem, ip: 0, mp: 0 }
    }

    fn run (&mut self) {
        let Prog(code) = &self.prog;
        loop {
            if self.ip >= code.len() {
                break
            }
            let op = &code[self.ip];
            op.exec(self);
            self.ip += 1;
        }
    }
}

#[derive(Debug)]
struct Mem (Vec<Cell>);

impl Mem {
    fn read(&mut self,index: usize) -> &mut Cell {
        &mut self.0[index]
    }
}

#[derive(Debug)]
struct Cell { byte: u8 }

impl Clone for Cell {
    fn clone (&self) -> Cell {
        Cell { byte: self.byte }
    }

}

impl Cell {
    fn new(byte: u8) -> Cell {
        Cell { byte }
    }
    fn increment (&mut self) {
        if self.byte == 255 { self.byte = 0 } else {
            self.byte += 1
        }
    }
    fn decrement (&mut self) {
        if self.byte == 0 { self.byte = 255 } else {
            self.byte -= 1
        }
    }
    fn zero (&self) -> bool {
        self.byte == 0
    }
    fn putchar (&self) {
        let char = std::char::from_u32(self.byte.into()).expect("u32 is a char");
        //println!("output: byte={}, '{}'",self.byte,char);
        print!("{}",char);
    }
    fn getchar (&mut self) {
        let byte : u8 =
            match std::io::stdin().bytes().next() {
                None => { 0 }
                Some(x) => { x.unwrap() }
            };
        self.byte = byte
    }
}
