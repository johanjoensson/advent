use std::fmt;
use std::cmp;

struct Point{
    x: i32,
    y: i32
}

impl Point{
    pub fn new(x: i32, y: i32) -> Point{
        Point{x: x, y: y}
    }
}

impl fmt::Display for Point{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(x : {}, y : {})", self.x, self.y)
    }
}
struct Line{
    start: Point,
    stop: Point
}
impl Line{
    pub fn new(start: Point, stop: Point) -> Line{
        Line { start: start, stop: stop}
    }
}
impl fmt::Display for Line{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.start, self.stop)
    }
}

struct Map{
    width: usize,
    height: usize,
    offset: Point,
    data: Vec<u32>
}

impl Map{
    pub fn new(width: usize, height: usize, offset: Point) -> Map{
        println!("Map width {}, height {}, offset {}", width, height, offset);
        Map {width: width, height: height, offset: offset, data: (0..width*height).map(|_| 0).collect()}
    }

    fn get(&self, x: i32, y: i32) -> u32{
        self.data[((y - self.offset.y) as usize)*self.width + (x - self.offset.x) as usize]
    }

    fn mark(&mut self, x: i32, y: i32){
        self.data[((y - self.offset.y) as usize)*self.width + (x - self.offset.x) as usize] += 1;
    }

    fn mark_line(&mut self, line : &Line){
    // plotLine(int x0, int y0, int x1, int y1)
    //     dx =  abs(x1-x0);
    //     sx = x0<x1 ? 1 : -1;
    //     dy = -abs(y1-y0);
    //     sy = y0<y1 ? 1 : -1;
    //     err = dx+dy;  /* error value e_xy */
    //     while (true)   /* loop */
    //         plot(x0, y0);
    //         if (x0 == x1 && y0 == y1) break;
    //         e2 = 2*err;
    //         if (e2 >= dy) /* e_xy+e_x > 0 */
    //             err += dy;
    //             x0 += sx;
    //         end if
    //         if (e2 <= dx) /* e_xy+e_y < 0 */
    //             err += dx;
    //             y0 += sy;
    //         end if
    //     end while
        let dx = (line.stop.x - line.start.x).abs();
        let dy = -(line.stop.y - line.start.y).abs();

        let step_x = if line.stop.x - line.start.x > 0 { 1 } else { -1 };
        let step_y = if line.stop.y - line.start.y > 0 { 1 } else { -1 };

        let mut d = dx + dy;
        let mut x = line.start.x;
        let mut y = line.start.y;
        self.mark(x, y);
        while x != line.stop.x || y != line.stop.y {
            let d2 = 2*d;
            if d2 >= dy{
                d += dy;
                x += step_x;
            }
            if d2 <= dx{
                d += dx;
                y += step_y;
            }
            if (x - self.offset.x) as usize >= self.width || (y - self.offset.y) as usize >= self.height{
                println!("{}", line);
                println!("{}", Point{x:x, y:y});
            }
            self.mark(x, y);
        }
    }
}

impl fmt::Display for Map{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in 0..self.height{
            for column in 0..self.width{
                if self.data[row*self.width + column] == 0{
                    write!(f, " . ");
                }else{
                    write!(f, " {} ", self.data[row*self.width + column]);
                }
            }
            write!(f, "\n");
        }
        write!(f, "")
    }
}

fn get_lines(s: &str) -> Vec<Line>{
    s.lines().map(|l| l.split(" -> ").collect::<Vec<&str>>()).map(
            |v| {
                let p0: Vec<i32> = v[0].split(",").map(|n| n.parse::<i32>()
                                                            .expect("error reading number!")).collect();
                let p1: Vec<i32> = v[1].split(",").map(|n| n.parse::<i32>()
                                                            .expect("error reading number!")).collect();
                Line{ start: Point{x : p0[0], y : p0[1]}, stop: Point{x : p1[0], y: p1[1]}}
            }
        ).collect::<Vec<Line>>()
}

pub fn run1(s: &str) -> i64{
    let lines = get_lines(s);

    let x_min = lines.iter().map(|l| cmp::min(l.start.x, l.stop.x)).min().expect("error finding minimum x-value.");
    let x_max = lines.iter().map(|l| cmp::max(l.start.x, l.stop.x)).max().expect("error finding maximum x-value.");
    let y_min = lines.iter().map(|l| cmp::min(l.start.y, l.stop.y)).min().expect("error finding minimum y-value.");
    let y_max = lines.iter().map(|l| cmp::max(l.start.y, l.stop.y)).max().expect("error finding maximum y-value.");

    let mut map = Map::new((x_max-x_min + 1) as usize, (y_max - y_min + 1) as usize, Point {x : x_min as i32, y: y_min as i32});

    
    for line in lines.iter()
                     .filter(|l| l.start.x == l.stop.x || l.start.y == l.stop.y){
        map.mark_line(&line);
    }
    let mut num_crossings = 0;
    for x in x_min..=x_max{
        for y in y_min..= y_max{
            if map.get(x, y) >= 2{
                num_crossings += 1;
            }
        }
    }

    num_crossings
}

pub fn run2(s: &str) -> i64{
    let lines = get_lines(s);

    let x_min = lines.iter().map(|l| cmp::min(l.start.x, l.stop.x)).min().expect("error finding minimum x-value.");
    let x_max = lines.iter().map(|l| cmp::max(l.start.x, l.stop.x)).max().expect("error finding maximum x-value.");
    let y_min = lines.iter().map(|l| cmp::min(l.start.y, l.stop.y)).min().expect("error finding minimum y-value.");
    let y_max = lines.iter().map(|l| cmp::max(l.start.y, l.stop.y)).max().expect("error finding maximum y-value.");

    let mut map = Map::new((x_max-x_min + 1) as usize, (y_max - y_min + 1) as usize, Point {x : x_min as i32, y: y_min as i32});

    
    for line in lines{
        map.mark_line(&line);
    }
    let mut num_crossings = 0;
    for x in x_min..=x_max{
        for y in y_min..= y_max{
            if map.get(x, y) >= 2{
                num_crossings += 1;
            }
        }
    }

    num_crossings
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
