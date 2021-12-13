use day3;
use std::fs;

fn main(){
    let filename = "input.txt";
    let contents = fs::read_to_string(filename)
                   .expect("Could not read the file!");
    
    let ans = day3::run1(&contents);
    println!("The product of the gamma and epsilon rates is {}", ans);
    let ans = day3::run2(&contents);
    println!("The product of the O2 generation rating and the CO2 scrubber rating is {}", ans);
}
