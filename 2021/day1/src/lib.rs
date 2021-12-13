use std::fs;

pub fn run(){
    println!("This is day one of many");
    let filename = "depths.txt";

    let contents: Vec<i32> = fs::read_to_string(filename)
        .expect("Could not read the file!")
        .lines()
        .map(|s| s.parse().unwrap())
        .collect();
    let increases = check_increase(&contents);
    println!("Number of depth increases: {}", 
             increases.iter()
                      .filter(|&b| *b)
                      .count());
    let partial_sums = sliding_window(&contents, 3);
    let partial_sums_increases = check_increase(&partial_sums);
    println!("Number of depth increases with sliding sums: {}", 
             partial_sums_increases.iter()
                      .filter(|&b| *b)
                      .count());
}

fn check_increase( depth: &Vec<i32>) -> Vec<bool>{
    let mut res = Vec::new();
    for i in 1..depth.len(){
        res.push(depth[i] > depth[i - 1]);
    }
    res
}

fn sliding_window( depth: &Vec<i32>, window_size: usize) -> Vec<i32>{
    let mut res = Vec::new();
    for i in window_size / 2 .. depth.len()  - (window_size / 2) {
        res.push(depth[i - window_size / 2 .. i + window_size / 2 + 1].iter().sum());
    }
    println! ("{:#?}", res);
    res
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
