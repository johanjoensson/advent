use std::fmt;

struct Display {
    segments: [bool; 8],
}

impl Display{
    pub fn new()->Display{
        Display{segments : [false, false, false, false, false, false, false, false]}
    }
    pub fn from_str(s: &str) -> Display{
        let mut res = Display::new();
        for segment in s.chars().map(|c| match c{
                              'a' => 0,
                              'b' => 1,
                              'c' => 2,
                              'd' => 3,
                              'e' => 4,
                              'f' => 5,
                              'g' =>6,
                              _ => panic!("Unknown segment!")
                          }
                     ).collect::<Vec<usize>>(){
            res.segments[segment] = true;
        }
        res
    }
    pub fn count_set_segments(&self) -> i32{
        self.segments.iter().fold(0, |acc, s| if *s { acc + 1} else {acc})
    }

}


impl fmt::Display for Display{
    fn fmt(&self, fo: &mut fmt::Formatter<'_>) -> fmt::Result {
        let a = if self.segments[0]{ 'a'} else {'.'};
        let b = if self.segments[1]{ 'b'} else {'.'};
        let c = if self.segments[2]{ 'c'} else {'.'};
        let d = if self.segments[3]{ 'd'} else {'.'};
        let e = if self.segments[4]{ 'e'} else {'.'};
        let f = if self.segments[5]{ 'f'} else {'.'};
        let g = if self.segments[6]{ 'g'} else {'.'};

        write!(fo, " {}{}{}{} \n", a, a, a, a);
        write!(fo, "{}    {}\n", b, c);
        write!(fo, "{}    {}\n", b, c);
        write!(fo, " {}{}{}{} \n", d, d, d, d);
        write!(fo, "{}    {}\n", e, f);
        write!(fo, "{}    {}\n", e, f);
        write!(fo, " {}{}{}{} \n", g, g, g, g)
    }
}
fn read_input_output(s: &str) -> (Vec<Vec<Display>>, Vec<Vec<Display>>){
    let mut inputs = Vec::new();
    let mut outputs = Vec::new();
    s.lines().for_each(|l| {let v = l.split("|").collect::<Vec<&str>>();
                            inputs.push(v[0].split_whitespace().map(|d| Display::from_str(d)).collect());
                            outputs.push(v[1].split_whitespace().map(|d| Display::from_str(d)).collect());
                           }
                 );

    (inputs, outputs)
}

pub fn run1(s: &str)-> i64{
    let (_, outputs) = read_input_output(s);
    let mut count = 0;
    for output in &outputs{
        for d in output{
            match d.count_set_segments(){
                2 | 3 | 4 | 7 => count += 1,
                _ => ()
            };
        }
    }
    count
}
