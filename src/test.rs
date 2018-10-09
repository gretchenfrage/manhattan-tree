
extern crate rand;

use self::rand::random;
use bounds::CompBounds;

#[test]
fn bounds_fuzz_test() {
    for _ in 0..500 {
        let mut bounds = CompBounds::<i64>::new();
        for _ in 0..1000 {
            let elem = [random::<i64>() % 100, random::<i64>() % 100, random::<i64>() % 100];
            bounds.add(elem);
            if random::<u8>() % 10 == 0 {
                bounds.remove(elem);
            }
        }
        assert_eq!(bounds.min_x().unwrap(),
                   bounds.cloud().iter().map(|&[x, _, _]| x).min().unwrap());
        assert_eq!(bounds.min_y().unwrap(),
                   bounds.cloud().iter().map(|&[_, y, _]| y).min().unwrap());
        assert_eq!(bounds.min_z().unwrap(),
                   bounds.cloud().iter().map(|&[_, _, z]| z).min().unwrap());

        assert_eq!(bounds.max_x().unwrap(),
                   bounds.cloud().iter().map(|&[x, _, _]| x).max().unwrap());
        assert_eq!(bounds.max_y().unwrap(),
                   bounds.cloud().iter().map(|&[_, y, _]| y).max().unwrap());
        assert_eq!(bounds.max_z().unwrap(),
                   bounds.cloud().iter().map(|&[_, _, z]| z).max().unwrap());
    }
}