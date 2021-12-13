use std::fmt::format;

#[derive(Debug)]
enum Instruction{
    Forward(i64),
    Down(i64),
    Up(i64),
}

fn get_instruction(s: &str, d: i64) -> Option<Instruction> {
    match s {
        "forward" => Some(Instruction::Forward(d)),
        "down" => Some(Instruction::Down(d)),
        "up" => Some(Instruction::Up(d)),
        _ =>None,
    }
}

fn get_instructions(s :&str) -> Vec<Instruction>{
        s.lines()
        .filter(|s| !s.chars().filter(|c| !c.is_whitespace())
                      .collect::<String>()
                      .is_empty()
               )
        .map(|s: &str| {
                 let mut it = s.split_whitespace();
                 let ins = it.next().expect("Expected a string first");
                 let dist = it.next()
                           .expect("Expected second argument on line!")
                           .parse()
                           .expect("Expected second argument to be an integer");
                 get_instruction(ins, dist).expect("Unknown instruction!")
                 }
            )
        .collect()
}

#[derive(Debug)]
struct Position{
    forward: i64,
    depth: i64,
}

impl Position {
    fn new(forward: i64, depth: i64) -> Position {
        Position {forward: forward, depth: depth}
    }
}

#[derive(Debug)]
pub struct Submarine{
   pos: Position, 
   aim: i64
}

impl Submarine {
    pub fn new() -> Submarine {
        Submarine {pos: Position::new(0, 0), aim: 0}
    }

    fn displace(&mut self, ins: Instruction){
        if let Instruction::Forward(dist) = ins{
            self.pos.forward += dist;
        }else if let Instruction::Down(dist) = ins{
            self.pos.depth += dist;
        }else if let Instruction::Up(dist) = ins{
            self.pos.depth -= dist;
        }
    }

    fn displace2(&mut self, ins: Instruction){
        if let Instruction::Forward(dist) = ins{
            self.pos.forward += dist;
            self.pos.depth += dist*self.aim;
        }else if let Instruction::Down(dist) = ins{
            self.aim += dist;
        }else if let Instruction::Up(dist) = ins{
            self.aim -= dist;
        }
    }
}


pub fn run1(s: &str ) -> i64{
    let instructions = get_instructions(s);
    let mut sub = Submarine::new();
    for ins in instructions{
        sub.displace(ins);
    }
    println!("{:#?}", sub);
    sub.pos.forward*sub.pos.depth
}
pub fn run2(s: &str ) -> i64{
    let instructions = get_instructions(s);
    let mut sub = Submarine::new();
    for ins in instructions{
        sub.displace2(ins);
    }
    println!("{:#?}", sub);
    sub.pos.forward*sub.pos.depth
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
