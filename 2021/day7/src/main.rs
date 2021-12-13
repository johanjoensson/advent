use std::fs;
use day7;

fn main() {
    let filename = "input.txt";
    let contents = fs::read_to_string(filename).expect("Error reading file!");

    let fuel = day7::run1(&contents);
    println!("The least amount of fuel spent to align the crabs is {}", fuel);

    let fuel = day7::run2(&contents);
    println!("The least amount of fuel spent to align the crabs is {}", fuel);
}
