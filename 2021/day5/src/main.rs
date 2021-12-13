use day5;
use std::fs;

fn main(){
    let filename = "input.txt";
    let contents = fs::read_to_string(filename).expect("Error reading file.");
    let ans = day5::run1(&contents);
    println!("Number of crossings {}", ans);
    let ans = day5::run2(&contents);
    println!("Number of crossings {}", ans);
    ()
}
