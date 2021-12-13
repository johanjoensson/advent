
fn get_crab_positions(s: &str) -> Vec<i64>{
    s.split(",")
     .map(|n| n.trim().parse::<i64>().expect("Error reading number!"))
     .collect::<Vec<i64>>()
}

fn get_median_position(positions: &Vec<i64>) -> i64{
    let N = positions.len();
    let mut p = positions.clone();
    p.sort();
    if N % 2 == 0{
        (0.5*((p[N/2] + p[N/2 - 1]) as f64)).round() as i64
    }else{
        p[N/2]
    }
}

fn get_average_position(positions: &Vec<i64>) -> f64{
    positions.iter().map(|i| *i as f64).sum::<f64>()/positions.len() as f64
}

fn get_total_position_difference(positions: &Vec<i64>, reference_position: i64) -> u64{
    positions.iter().fold(0, |acc, p| acc + (p - reference_position).abs().abs() as u64)
}

fn total_fuel_consumed(positions: &Vec<i64>, align_position: i64) -> u64{
    positions.iter().fold(0, |acc, pos| acc + (0..=(pos - align_position).abs()).sum::<i64>()) as u64
    // positions.iter().fold(0, |acc, pos| acc + ((pos - align_position).abs() as f64 / 2. * (((pos - align_position).abs() + 1) as f64)) as u64)
}

pub fn run1(s: &str) -> u64{
    let crab_positions = get_crab_positions(s);
    let median_position = get_median_position(&crab_positions);
    // let avg_position = get_average_position(&crab_positions);
    println!("Median crab position: {}", median_position);
    get_total_position_difference(&crab_positions, median_position)
}

pub fn run2(s: &str) -> u64{
    let crab_positions = get_crab_positions(s);
    let mut candidate_positions = crab_positions.clone();
    candidate_positions.sort();
    candidate_positions.dedup();
    let first_candidate = *candidate_positions.first().unwrap();
    let last_candidate = *candidate_positions.last().unwrap();
    println!("Search for align position in the range {} to {}", first_candidate, last_candidate);
    let mut fuel_costs = Vec::new();
    for candidate in first_candidate..=last_candidate{
        fuel_costs.push(total_fuel_consumed(&crab_positions, candidate));
    }
    let (idx, fuel) = fuel_costs.iter().enumerate().min_by(|(_,f1),(_, f2)| f1.cmp(f2)).expect("Error finding minimum fuel cost!");
    println!("Align position should be {}", idx + first_candidate as usize);
    *fuel
}
