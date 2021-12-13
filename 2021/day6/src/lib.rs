
struct FishList{
    fishbins: [usize; 9],
    new_age: usize,
    reset_age: usize
}

fn get_population(s: &str) -> Vec<usize>{
    s.split(",").map(|n| n.trim().parse().expect("Error reading number!")).collect()
}

impl FishList{
    fn new(fish_population: Vec<usize>) -> FishList{
        let mut fishbins = [0, 0, 0, 0, 0, 0, 0, 0, 0];
        for fish_age in fish_population{
            fishbins[fish_age] += 1;
        }
        FishList{ fishbins: fishbins, new_age: 8, reset_age: 6}
    }
    
    fn age(&mut self){
        let new_fish = self.fishbins[0];
        let reset_fish = self.fishbins[0];
        for age in 1..self.fishbins.len(){
            self.fishbins[age -1] = self.fishbins[age];
        }
        self.fishbins[self.new_age] = new_fish;
        self.fishbins[self.reset_age] += reset_fish;
    }

    fn num_fish(&self) -> usize{
        self.fishbins.iter().sum()
    }
}

pub fn run1(contents: &str, days: usize) -> usize{
    let mut fish = FishList::new(get_population(contents));
    println!("Starting fish population is {}", fish.num_fish());
    for _ in 0..days{
        fish.age();
    }
    fish.num_fish()
}
