use std::ops::Index;
use std::fmt;

#[derive(Debug)]
struct Board{
   data : Vec<i64>,
   marks : Vec<(usize, usize)>,
   rows : usize,
   columns : usize,
}

impl Board{
    fn new(num_rows: usize, num_columns: usize) -> Board{
        let data = (0..(num_rows*num_columns)).map(|_| 0).collect();
        Board{data : data, marks : Vec::new(), rows : num_rows, columns : num_columns}
    }
    fn set(&mut self, row : usize, column : usize, val : i64){
        self.data[row*self.columns + column] = val;
    }
    fn get(&self, row : usize, column : usize) -> i64{
        self.data[row*self.columns + column]
    }
    fn mark(&mut self, row : usize, column : usize){
        self.marks.push((row, column));
    }
}

impl fmt::Display for Board{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in 0..self.rows{
            for column in 0..self.columns{
                if !self.marks.contains(&(row, column)){
                    write!(f, "{} ", self.get(row, column));
                }else{
                    write!(f, "*{} ", self.get(row, column));
                }
            }
            write!(f, "\n");
        }
        write!(f, "")
    }
}

fn get_numbers(s: &str) -> Vec<i64>{
    s.split(",")
     .map(|n| n.parse().expect("Could not read number!"))
     .collect()
}

use std::str::Lines;
fn get_boards(mut lines : Lines) -> Vec<Board>{
    let mut boards = Vec::new();
    while let (Some(l0), Some(l1), Some(l2), Some(l3), Some(l4)) = (lines.next(), lines.next(), lines.next(), lines.next(), lines.next()){
        let mut board = Board::new(5, 5);
        let mut ri = 0;
        for l in [l0, l1, l2, l3, l4]{
            let mut ci = 0;
            let vals: Vec<i64> = l.split_whitespace()
                                  .map(|n| n.parse().expect("Could not read number!"))
                                  .collect();
            for val in vals {
                board.set(ri, ci, val);
                ci += 1;
            }
            ri += 1
        }
        boards.push(board);
        lines.next();
    }
    boards
}

fn mark_number_on_boards(boards : Vec<Board>, val: i64) -> Vec<Board> {
    let mut res = Vec::new();
    for mut board in boards{
        for ri in 0..board.rows{
            for ci in 0..board.columns{
                if board.get(ri, ci) == val{
                    board.mark(ri, ci);
                }
            }
        }
        res.push(board);
    }
    res
}

fn check_winner(board: &Board) -> bool{
    let mut winner = false;
    for row in 0..board.rows{
        for column in 0..board.columns{
            if !board.marks.contains(&(row, column)){
                winner = false;
                break;
            }
            winner = true;
        }
        if winner{
            break;
        }
    }
    if ! winner{
        for column in 0..board.columns{
            for row in 0..board.rows{
                if !board.marks.contains(&(row, column)){
                    winner = false;
                    break;
                }
                winner = true;
            }
            if winner{
                break;
            }
        }
    }
    winner
}

fn check_boards(boards: &Vec<Board>) -> Option<&Board>{
    boards.iter().find(|b| check_winner(b))
}

fn sum_unmarked(board: &Board) -> i64{
    let mut sum = 0;
    for row in 0..board.rows{
        for column in 0..board.columns{
            if !board.marks.contains(&(row, column)){
                sum += board.get(row, column);
            }
        }
    }
    sum
}

pub fn run1(s: &str) -> i64{
    let mut lines = s.lines();
    let numbers = get_numbers(&lines.next().expect("Could not read first line of input!"));
    lines.next();
    let mut boards = get_boards(lines);
    for i in 0..4{
        let num = numbers[i];
        boards = mark_number_on_boards(boards, num);
    }

    let mut ans = 0;
    for i in 4..numbers.len(){
        let num = numbers[i];
        boards = mark_number_on_boards(boards, num);
        if let Some(winner) = check_boards(&boards){
            let usum = sum_unmarked(winner);
            println!("{}", winner);
            println!("Number {}, sum of unmarked number on winning board {}", num, usum);
            ans = usum * num;
            break;
        }
    }
    ans
}

pub fn run2(s: &str) -> i64{
    let mut lines = s.lines();
    let numbers = get_numbers(&lines.next().expect("Could not read first line of input!"));
    lines.next();
    let mut boards = get_boards(lines);
    for i in 0..4{
        let num = numbers[i];
        boards = mark_number_on_boards(boards, num);
    }

    let mut ans = 0;
    for i in 4..numbers.len(){
        let num = numbers[i];
        boards = mark_number_on_boards(boards, num);
        if boards.len() > 1{
            boards.retain(|b| !check_winner(b));
        }else{
            if let Some(winner) = check_boards(&boards){
                let usum = sum_unmarked(winner);
                println!("{}", winner);
                println!("Number {}, sum of unmarked number on last winning board {}", num, usum);
                ans = usum * num;
                break;
            }
        }
    }
    ans
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
