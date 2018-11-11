
extern crate rand;

use super::space::*;
use super::collections::*;
use super::tree::coord::BaseCoord;

use std::collections::HashMap;
use std::ops::Rem;

use self::rand::{*, XorShiftRng};
use self::rand::distributions::{Standard, Distribution};

#[test]
fn fill_remove_fill() {
    let mut tree = MTreeQueue::new(U56Space);
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
    let mut tree = MTreeQueue::new(U56Space);
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

    let mut tree = MTreeQueue::new(U56Space);

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

fn set_test<C: Rem<C, Output = C>, S: CoordSpace<Coord = [C; 3]> + Clone>(space: S, max: C)
    where
        Standard: Distribution<C>,
        C: Copy + Clone,
        [C; 3]: Copy + Clone {
    let seed = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
    let mut rng = XorShiftRng::from_seed(seed);

    let mut tree: MTreeSet<S> = MTreeSet::new(space.clone());
    let mut naive: HashMap<[u64; 3], [C; 3]> = HashMap::new();

    let rng_coord = |rng: &mut XorShiftRng| [
        rng.gen::<C>() % max,
        rng.gen::<C>() % max,
        rng.gen::<C>() % max,
    ];

    for _i in 0..5000 {
        match rng.gen::<u8>() % 5 {
            0 => {
                // insert
                let coord = rng_coord(&mut rng);
                tree.insert(coord);
                naive.insert(space.raw(coord), coord);
            },
            1 => {
                // remove
                let coord = rng_coord(&mut rng);
                tree.remove(coord);
                naive.remove(&space.raw(coord));
            },
            2 => {
                // remove closest
                let focus = rng_coord(&mut rng);
                if let Some(coord) = tree.remove_closest(focus) {
                    naive.remove(&space.raw(coord));
                }
            },
            3 => {
                // verify closest
                let focus = rng_coord(&mut rng);
                let focus_coord = BaseCoord {
                    comps: space.raw(focus)
                };

                let fast_closest = tree.closest(focus).cloned();
                let naive_closest = {
                    let mut vec: Vec<[u64; 3]> = naive.iter().map(|(&a, _)| a).collect();
                    vec.sort_by_key(|&elem| BaseCoord {
                        comps: elem
                    }.manhattan_dist(focus_coord));
                    vec.into_iter().next()
                };

                assert_eq!(
                    fast_closest
                        .map(|comps| BaseCoord { comps: space.raw(comps) }.manhattan_dist(focus_coord)),
                    naive_closest
                        .map(|comps| BaseCoord { comps }.manhattan_dist(focus_coord)),
                );
            },
            4 => {
                // assert consistency
                let mut t_size = 0;
                for &coord in tree.iter() {
                    assert!(tree.contains(coord));
                    assert!(naive.contains_key(&space.raw(coord)));
                    t_size += 1;
                }
                let mut n_size = 0;
                for (_, &coord) in &naive {
                    assert!(tree.contains(coord));
                    assert!(naive.contains_key(&space.raw(coord)));
                    n_size += 1;
                }
                assert_eq!(t_size, n_size);
                //println!("consistent at {}", _i);
            }
            _ => unreachable!(),
        }
    }
}

macro_rules! set_test {
    ($name:ident, $space:expr, $max:expr) => {
        #[test]
        fn $name() {
            set_test($space, $max);
        }
    }
}

set_test!(set_test_u64, U56Space, 16);
set_test!(set_test_u32, U32Space, 16);
set_test!(set_test_i64, I56Space, 16);
set_test!(set_test_i32, I32Space, 16);
set_test!(set_test_f64, F64Space { range: 16.0 }, 16.0);
set_test!(set_test_f32, F32Space { range: 16.0 }, 16.0);