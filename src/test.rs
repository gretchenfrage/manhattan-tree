
use super::*;

#[test]
fn fill_remove_fill() {
    let mut tree = Octree::new();
    for i in 0..3 {
        println!("adding {}", i);
        tree.add([i, i, i], i);
    }
    for i in 0..3 {
        println!("removing {}", i);
        tree.remove_closest([0, 0, 0]).unwrap();
    }
    for i in 3..6 {
        println!("adding {}", i);
        tree.add([i, i, i], i);
    }
}