use std::fs;

fn seq_sum(s: i32, n: i32) -> i32 {
    return (s + (s+(n-1)))*n/2;
}

fn main() {
    let mut v:Vec<i32> = fs::read_to_string("./input.txt").unwrap().split(',').map(|x| x.parse().unwrap()).collect();
    v.sort();

    // Part A
    {
        let mid = v[v.len()/2];
        let mut fuel = 0i32;
        for x in v.iter() {
            fuel += (mid - x).abs();
        }
        println!("fuelA {}", fuel);
    }

    // Part B
    {
        let mut fuels = Vec::<i32>::new();
        let m = *v.iter().max().unwrap();
        for i in 0..m {
            let mut fuel = 0i32;
            for x in v.iter() {
                let c = (i - x).abs();
                if c > 0 {
                    fuel += seq_sum(1, c);
                }
            }

            fuels.push(fuel);
        }

        fuels.sort();
        println!("fuelB {:?}", fuels[0]);
    }
}
