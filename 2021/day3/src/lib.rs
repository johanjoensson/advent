
fn get_numbers(s: &str) -> Vec<Vec<u32>>{
    s.lines()
        .filter(|l| !l.chars().filter(|c| !c.is_whitespace())
                      .collect::<String>()
                      .is_empty()
               )
        .map(|s| s.chars().map(|c| c.to_digit(2)
                                    .expect("Error reading character"))
                                    .collect())
        .collect()
}

pub fn run1(s:&str) -> i64{
    let nums = get_numbers(&s);
    let mut com = Vec::new();
    for _ in &nums[0]{
        com.push(0);
    }
    for i in 0..com.len(){
        com[i] = get_most_common(&nums, i);
    }
    let mut gamma = 0;
    let mut epsilon = 0;
    for n in com{
        gamma = 2*gamma + n;
        epsilon = 2*epsilon + (1 - n);
    }
    return (epsilon*gamma) as i64
}

fn get_most_common(nums: &Vec<Vec<u32>>, i: usize) -> u32{
    let mut val = 0;
    for num in nums{
        val += 2*(num[i] as i32) - 1;
    }
    if val >= 0{
        1
    }else{
        0
    }
}
pub fn run2(s: &str) -> i64{
    let nums = get_numbers(&s);
    let mut candidates: Vec<Vec<u32>> = nums.clone();
    for i in 0..candidates[0].len(){
        println!("Number of candidates: {}", candidates.len());
        if candidates.len() == 1 {
            break;
        }
        let v = get_most_common(&candidates, i);
        println!("Most common value a position {}: {}", i, v);
        candidates = candidates.into_iter()
                               .filter(|num| num[i] == v)
                               .collect();
    }
    let mut o2_gen = 0;
    for &c in &candidates[0]{
        o2_gen = 2*o2_gen + c;
    }
    println!("O2 generation rating is {}", o2_gen);

    let mut candidates: Vec<Vec<u32>> = nums.clone();
    for i in 0..candidates[0].len(){
        println!("Number of candidates: {}", candidates.len());
        if candidates.len() == 1 {
            break;
        }
        let v = get_most_common(&candidates, i);
        println!("Most common value a position {}: {}", i, v);
        candidates = candidates.into_iter()
                               .filter(|num| num[i] == 1 - v)
                               .collect();
    }
    let mut co2_scrub = 0;
    for &c in &candidates[0]{
        co2_scrub = 2*co2_scrub + c;
    }
    println!("CO2 scrubber rating is {}", co2_scrub);
    (o2_gen*co2_scrub) as i64
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
