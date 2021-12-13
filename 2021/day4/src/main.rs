use day4;
use std::fs;
use std::format;

fn main(){
    let filename = "input.txt";
    let contents = fs::read_to_string(filename)
                   .expect(&format!("Could not open the file: {}", filename));
    let ans1 = day4::run1(&contents);
    println!("Sum of unmarked numbers times last number: {}", ans1);
    let ans2 = day4::run2(&contents);
    println!("Sum of unmarked numbers times last number for the last winning board: {}", ans2);
}
