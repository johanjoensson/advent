use day2;
use std::fs;

fn main(){
    let filename = "input.txt";
    let contents = fs::read_to_string(filename)
                   .expect("Could not read the file!");

    let res = day2::run1(&contents);
    println!("The answer is {}", res);
    let res = day2::run2(&contents);
    println!("The answer is {}", res);
}
