use std::fs;
use day8;

fn main() {
    let filename = "input.txt";
    let contents = fs::read_to_string(filename).expect("Error reading file!");
    let ans = day8::run1(&contents);

    println!("{} segments are either 1, 4, 7 or 8!", ans);
}
