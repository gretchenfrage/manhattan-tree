
extern crate rand;

use super::space::U63Space;
use super::collection::MTreeQueue;

use self::rand::{*, XorShiftRng};

#[test]
fn fill_remove_fill() {
    let mut tree = MTreeQueue::new(U63Space);
    for i in 0..3 {
        //println!("adding {}", i);
        tree.insert([i, i, i], i);
    }
    for _i in 0..3 {
        //println!("removing {}", i);
        tree.remove([0, 0, 0]).unwrap();
    }
    for i in 3..6 {
        //println!("adding {}", i);
        tree.insert([i, i, i], i);
    }
}

#[test]
fn fill_remove_queue() {
    let mut tree = MTreeQueue::new(U63Space);
    for i in 0..3 {
        tree.insert([i, i, i], i);
    }

    for _i in 0..2 {
        tree.insert([10, 10, 10], 10);
    }

    for _i in 0..2 {
        assert_eq!(tree.remove([10, 10, 10]), Some(10));
    }

    for _i in 0..2 {
        tree.insert([10, 10, 10], 20);
    }
}

#[test]
fn chaotic_fill_empty() {
    let seed = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
    let mut rng = XorShiftRng::from_seed(seed);

    let mut tree = MTreeQueue::new(U63Space);

    let rng_coord = |rng: &mut XorShiftRng| [
        rng.gen::<u64>() % 16,
        rng.gen::<u64>() % 16,
        rng.gen::<u64>() % 16,
    ];

    for i in 0..5000 {
        //println!("{}", i);
        if i == 38 {
            //println!("{}", tree.debug_nodes());
            //println!("{:#?}", tree);
        }
        if rng.gen::<bool>() {
            tree.insert(rng_coord(&mut rng), ());
        } else {
            for _ in 0..rng.gen::<u32>() % 2 + 1 {
                tree.remove(rng_coord(&mut rng));
            }
        }
    }
}