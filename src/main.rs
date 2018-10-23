#![feature(nll)]

extern crate num;
extern crate bonzai;

#[cfg(test)]
mod test;
mod smallqueue;
mod coord;
mod transform;
mod children;
mod bounds;

use smallqueue::SmallQueue;
use coord::*;
use children::*;
use bounds::*;

use std::fmt::{Debug, Formatter};
use std::fmt;
use std::mem;
use std::u64;

use bonzai::*;


pub struct Tree<T> {
    nodes: Vec<Octant<T>>,
    root: Option<usize>,
}
impl<T> Tree<T> {
    pub fn new() -> Self {
        Tree {
            nodes: Vec::new(),
            root: None
        }
    }

    fn add_node(&mut self, node: Octant<T>) -> usize {
        let i = self.nodes.len();
        self.nodes.push(node);
        i
    }

    pub fn add(&mut self, coord: impl Into<BaseCoord>, elem: T) {
        let coord = coord.into();
        let new_root = Octant::add(None, self.root, self, coord, elem);
        self.root = Some(new_root);
        //self.clean();
    }

    pub fn closest(&self, focus: impl Into<BaseCoord>) -> Option<BaseCoord> {
        let focus = focus.into();
        Octant::closest(
            self.root,
            self,
            focus,
            None
        )
    }

    /// Clean up Octant::Temp nodes, which requires a careful restructuring of the tree.
    pub fn clean(&mut self) {
        // until we've scanned through the entire vec
        let mut i = 0;
        while i < self.nodes.len() {
            // if the node has become temp, we should clean it up
            if match &self.nodes[i] {
                &Octant::Temp => true,
                _ => false
            } {
                // swap_remove it
                // however, this changes the position of the last node to i
                self.nodes.swap_remove(i);
                // get the parent and coord of the repositioned node
                let (parent_i, child_coord) = match &self.nodes[i] {
                    &Octant::Leaf {
                        parent,
                        coord,
                        ..
                    } => (parent, coord),
                    &Octant::Branch {
                        parent,
                        coord,
                        ..
                    } => (parent, coord.to_base()),
                    &Octant::Temp => {
                        // but if the repositioned node is also temp, then repeat this same
                        // iteration of the loop
                        i -= 1;
                        continue;
                    }
                };
                if let Some(parent_i) = parent_i {
                    // now we want to re-point the parent to to the child
                    if let &mut Octant::Branch {
                        coord: parent_coord,
                        children: ref mut parent_children,
                        ..
                    } = &mut self.nodes[parent_i] {
                        // we must us to determine which suboct the child is
                        let suboctant = parent_coord.suboctant(child_coord).unwrap();
                        // and then change the index of that child
                        *parent_children.get_mut(suboctant) = Some(i);

                        println!("cleaned!");
                    } else {
                        // this branch means that the parent of a node was not a branch
                        // which is illegal
                        unreachable!()
                    }
                } else {
                    // however, if we actually repositioned the root node, then we simply want
                    // to update our root index
                    self.root = Some(i);
                }
            }

            i += 1;
        }
    }
}

impl<T: Debug> Debug for Tree<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        OctDebug {
            nodes: &self.nodes,
            index: self.root
        }.fmt(f)
    }
}

enum Octant<T> {
    Leaf {
        coord: BaseCoord,
        elems: SmallQueue<T>,
        parent: Option<usize>,
    },
    Branch {
        coord: OctCoord,
        children: Children<Option<usize>>,
        bounds: CompBounds,
        parent: Option<usize>,
    },
    Temp,
}
impl<T> Octant<T> {
    fn new_leaf(parent: Option<usize>, coord: BaseCoord, elem: T) -> Self {
        Octant::Leaf {
            coord,
            elems: SmallQueue::of(elem),
            parent,
        }
    }

    fn new_branch(tree: &Tree<T>, coord: OctCoord, children: Children<Option<usize>>, parent: Option<usize>) -> Self {
        let mut bounds_elems = Vec::new(); // TODO: smalllvec!
        for &x in [Pole::N, Pole::P].iter() {
            for &y in [Pole::N, Pole::P].iter() {
                for &z in [Pole::N, Pole::P].iter() {
                    if let Some(bound_elem) = Octant::bounds(tree, *children.get([x, y, z])) {
                        bounds_elems.push(bound_elem);
                    }
                }
            }
        }
        let bounds = CompBounds::combine(&bounds_elems);
        Octant::Branch {
            coord,
            children,
            bounds,
            parent,
        }
    }

    fn bounds(tree: &Tree<T>, this_i: Option<usize>) -> Option<CompBounds> {
        match this_i.map(|i| &tree.nodes[i]) {
            Some(&Octant::Leaf {
                coord,
                ..
            }) => Some(CompBounds::of(coord)),
            Some(&Octant::Branch {
                bounds,
                ..
            }) => Some(bounds),
            Some(&Octant::Temp) | None => None,
        }
    }

    fn add(this_parent: Option<usize>, this_i: Option<usize>, tree: &mut Tree<T>, elem_coord: BaseCoord, elem: T) -> usize {
        let (this, this_i) = match this_i {
            Some(i) => (Some(mem::replace(&mut tree.nodes[i], Octant::Temp)), i),
            None => (None, tree.add_node(Octant::Temp))
        };
        let this = match this {
            None => {
                Octant::new_leaf(this_parent, elem_coord, elem)
            },
            Some(Octant::Leaf {
                coord: leaf_coord,
                elems: mut leaf_elems,
                parent: _,
            }) => {
                if leaf_coord == elem_coord {
                    leaf_elems.add(elem);
                    Octant::Leaf {
                        parent: this_parent,
                        coord: leaf_coord,
                        elems: leaf_elems,
                    }
                } else {
                    let branch_coord = elem_coord.lowest_common_octant(leaf_coord);

                    let old_suboctant = branch_coord.suboctant(leaf_coord).unwrap();
                    let old_child = Octant::Leaf {
                        parent: Some(this_i),
                        coord: leaf_coord,
                        elems: leaf_elems,
                    };
                    let old_child = tree.add_node(old_child);

                    let new_suboctant = branch_coord.suboctant(elem_coord).unwrap();
                    let new_child = Octant::new_leaf(Some(this_i), elem_coord, elem);
                    let new_child = tree.add_node(new_child);

                    let children = Children::new(|suboctant| {
                        if suboctant == old_suboctant {
                            Some(old_child)
                        } else if suboctant == new_suboctant {
                            Some(new_child)
                        } else {
                            None
                        }
                    });

                    //let bounds = CompBounds::combine(&[CompBounds::of(elem_coord), CompBounds::of(leaf_coord)]);

                    /*
                    Octant::Branch {
                        coord: branch_coord,
                        children,
                        bounds,
                        parent: this_parent,
                    }
                    */
                    Octant::new_branch(tree, branch_coord, children, this_parent)
                }
            },
            Some(Octant::Branch {
                coord: branch_coord,
                children: mut branch_children,
                ..
            }) => {
                if let Some(child_suboctant) = branch_coord.suboctant(elem_coord) {
                    let new_child = Octant::add(
                        Some(this_i),
                        *branch_children.get(child_suboctant), tree, elem_coord, elem);
                    *branch_children.get_mut(child_suboctant) = Some(new_child);
                    /*
                    if created {
                        bounds.add(elem_coord);
                    }
                    */
                    /*
                    (Octant::Branch {
                        coord: branch_coord,
                        children: branch_children,
                        bounds,
                        parent: this_parent,
                    }, created)
                    */
                    Octant::new_branch(tree, branch_coord, branch_children, this_parent)
                } else {
                    let old_branch_coord = branch_coord;
                    let new_branch_coord = old_branch_coord.to_base()
                        .lowest_common_octant(elem_coord);

                    /*
                    let mut new_branch_bounds = bounds.clone(); // TODO: this is damn expensive
                    new_branch_bounds.add(elem_coord);
                    */

                    let old_suboctant =
                        new_branch_coord.suboctant(old_branch_coord.to_base()).unwrap();
                    /*
                    let old_child = Octant::Branch {
                        coord: old_branch_coord,
                        children: branch_children,
                        bounds,
                        parent: Some(this_i),
                    };
                    */
                    let old_child = Octant::new_branch(tree, old_branch_coord, branch_children, Some(this_i));
                    let old_child = tree.add_node(old_child);

                    let new_suboctant = new_branch_coord.suboctant(elem_coord).unwrap();
                    let new_child = Octant::new_leaf(Some(this_i), elem_coord, elem);
                    let new_child = tree.add_node(new_child);

                    let children = Children::new(|suboctant| {
                        if suboctant == old_suboctant {
                            Some(old_child)
                        } else if suboctant == new_suboctant {
                            Some(new_child)
                        } else {
                            None
                        }
                    });

                    /*
                    Octant::Branch {
                        coord: new_branch_coord,
                        children,
                        bounds: new_branch_bounds,
                        parent: this_parent,
                    }
                    */
                    Octant::new_branch(tree, new_branch_coord, children, this_parent)
                }
            },
            Some(Octant::Temp) => unreachable!()
        };

        mem::replace(&mut tree.nodes[this_i], this);
        this_i
    }

    fn closest(this_i: Option<usize>, tree: &Tree<T>, focus: BaseCoord, competitor: Option<BaseCoord>) -> Option<BaseCoord> {
        let this = this_i.map(|i| &tree.nodes[i]);
        match this {
            None => None,
            Some(&Octant::Leaf {
                coord,
                ..
            }) => {
                if competitor
                    .map(|competitor|
                        competitor.manhattan_dist(focus) < coord.manhattan_dist(focus))
                    .unwrap_or(false) {

                    None
                } else {
                    Some(coord)
                }
            },
            Some(&Octant::Branch {
                coord,
                ref children,
                ref bounds,
                ..
            }) => {
                if let Some(competitor) = competitor {
                    debug_assert!(coord.suboctant(focus).is_none());

                    let closest_suboct = coord.closest_suboctant(focus);

                    // x axis short circuit
                    if closest_suboct[0] == Pole::N {
                        if (competitor.manhattan_dist(focus) as i64) <
                            bounds.min[0] as i64 - focus.comps[0] as i64 {
                            return None;
                        }
                    } else {
                        if (competitor.manhattan_dist(focus) as i64) <
                            focus.comps[0] as i64 - bounds.max[0] as i64 {
                            return None;
                        }
                    }
                    // y axis short circuit
                    if closest_suboct[1] == Pole::N {
                        if (competitor.manhattan_dist(focus) as i64) <
                            bounds.min[1] as i64 - focus.comps[1] as i64 {
                            return None;
                        }
                    } else {
                        if (competitor.manhattan_dist(focus) as i64) <
                            focus.comps[1] as i64 - bounds.max[1] as i64 {
                            return None;
                        }
                    }
                    // z axis short circuit
                    if closest_suboct[2] == Pole::N {
                        if (competitor.manhattan_dist(focus) as i64) <
                            bounds.min[2] as i64 - focus.comps[2] as i64 {
                            return None;
                        }
                    } else {
                        if (competitor.manhattan_dist(focus) as i64) <
                            focus.comps[2] as i64 - bounds.max[2] as i64 {
                            return None;
                        }
                    }

                    let mut best: Option<BaseCoord> = None;
                    suboct_search_from(
                        Some(closest_suboct),
                        true,
                        |suboct| {
                            if let Some(better) = Octant::closest(
                                *children.get(suboct),
                                tree,
                                focus,
                                Some(best.unwrap_or(competitor))
                            ) {
                                best = Some(better);
                            }
                        }
                    );

                    best
                } else {
                    let focused_suboct: Option<SubOctant> = coord.suboctant(focus);

                    let mut best: Option<BaseCoord> = focused_suboct
                        .and_then(|suboct| Octant::closest(
                            *children.get(suboct),
                            tree,
                            focus,
                            None
                        ));

                    suboct_search_from(
                        focused_suboct,
                        false,
                        |suboct| {
                            if let Some(better) = Octant::closest(
                                *children.get(suboct),
                                tree,
                                focus,
                                best
                            ) {
                                best = Some(better);
                            }
                        }
                    );

                    best
                }
            }
            Some(&Octant::Temp) => unreachable!()
        }
    }
}

struct OctDebug<'a, T: Debug> {
    nodes: &'a [Octant<T>],
    index: Option<usize>,
}
impl<'a, T: Debug> Debug for OctDebug<'a, T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        if let Some(index) = self.index {
            match &self.nodes[index] {
                &Octant::Leaf {
                    ref coord,
                    ref elems,
                    ..
                } => {
                    f.debug_struct("Leaf")
                        .field("coord", coord)
                        .field("elems", elems)
                        .finish()
                },
                &Octant::Branch {
                    ref coord,
                    ref children,
                    ..
                } => {
                    f.debug_struct("Branch")
                        .field("coord", coord)
                        .field("children", &BranchChildrenDebug {
                            nodes: self.nodes,
                            children
                        })
                        .finish()
                }
                &Octant::Temp => {
                    f.debug_struct("Temp")
                        .finish()
                }
            }
        } else {
            f.debug_struct("Empty")
                .finish()
        }
    }
}

struct BranchChildrenDebug<'a, T: Debug> {
    nodes: &'a [Octant<T>],
    children: &'a Children<Option<usize>>
}
impl<'a, T: Debug> Debug for BranchChildrenDebug<'a, T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("Children")
            .field("ppp", &OctDebug {
                nodes: self.nodes,
                index: *self.children.get([Pole::P, Pole::P, Pole::P])
            })
            .field("npp", &OctDebug {
                nodes: self.nodes,
                index: *self.children.get([Pole::N, Pole::P, Pole::P])
            })
            .field("pnp", &OctDebug {
                nodes: self.nodes,
                index: *self.children.get([Pole::P, Pole::N, Pole::P])
            })
            .field("ppn", &OctDebug {
                nodes: self.nodes,
                index: *self.children.get([Pole::P, Pole::P, Pole::N])
            })
            .field("pnn", &OctDebug {
                nodes: self.nodes,
                index: *self.children.get([Pole::P, Pole::N, Pole::N])
            })
            .field("npn", &OctDebug {
                nodes: self.nodes,
                index: *self.children.get([Pole::N, Pole::P, Pole::N])
            })
            .field("nnp", &OctDebug {
                nodes: self.nodes,
                index: *self.children.get([Pole::N, Pole::N, Pole::P])
            })
            .field("nnn", &OctDebug {
                nodes: self.nodes,
                index: *self.children.get([Pole::N, Pole::N, Pole::N])
            })
            .finish()
    }
}

#[cfg(not(test))]
extern crate rand;
extern crate stopwatch;

use stopwatch::Stopwatch;

use rand::prng::XorShiftRng;
use rand::{Rng, SeedableRng};

fn main() {
    let mut tree: Tree<()> = Tree::new();
    //let mut elems: Vec<[u64; 3]> = Vec::new();

    let mut rng: XorShiftRng = SeedableRng::from_seed(
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]);

    let mut timer = Stopwatch::start_new();

    for i in 0..1000000 {
        let elem = [rng.gen::<u64>() / 8, rng.gen::<u64>() / 8, rng.gen::<u64>() / 8];
        tree.add(elem, ());
        if i % 1000 == 0 {
            //println!("inserting element i={}", i);
        }
        //elems.push(elem);
    }

    println!("inserted in {}s", timer.elapsed_ms() as f64 / 1000.0);
    timer.restart();

    let mut rng: XorShiftRng = SeedableRng::from_seed(
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]);

    for i in 0..1000000 {
        let focus = [rng.gen::<u64>() / 8, rng.gen::<u64>() / 8, rng.gen::<u64>() / 8];

        let _tree_closest: [u64; 3] = tree.closest(focus).unwrap().into();

        /*
        elems.sort_by_key(|&elem| BaseCoord::from(elem).manhattan_dist(focus.into()));
        let vec_closest = elems.iter().next().cloned().unwrap();

        if BaseCoord::from(tree_closest).manhattan_dist(focus.into()) !=
            BaseCoord::from(vec_closest).manhattan_dist(focus.into()) {

            eprintln!();
            eprintln!("!!!!!!!!!!!incorrect closest (i={}):", i);
            eprintln!("focus = {:?}", focus);
            eprintln!("tree closest = {:?}", tree_closest);
            eprintln!("vec closest = {:?}", vec_closest);
            eprintln!();
        } else {
            //println!("correct! (i={}, focus={:?})", i, focus);
        }
        */

        if i % 1000 == 0 {
            //println!("queried! i={}", i);
        }
    }

    println!("queried in {}s", timer.elapsed_ms() as f64 / 1000.0);

    println!("done!");

}
