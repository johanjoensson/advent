use day6;
use std::fs;

fn main() {
    let filename = "input.txt";
    let contents = fs::read_to_string(filename).expect("Error reading file!");
    let fish_population = day6::run1(&contents, 256);
    println!("After 80 days the fish population is {}", fish_population);
}
