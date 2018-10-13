
#![feature(nll)]

extern crate num;

#[cfg(test)]
mod test;
mod bounds;
mod smallqueue;

use bounds::CompBounds;
use smallqueue::SmallQueue;

use std::cmp::{min, max};
use std::fmt::{Debug, Formatter};
use std::fmt;
use std::ptr;
use std::ops::Not;
use std::mem;

use num::abs;
use num::Integer;

/// Mutate a referenced element by transferring ownership through a function.
fn replace<T>(elem: &mut T, func: impl FnOnce(T) -> T) {
    unsafe {
        let elem_ref = elem;
        let elem = ptr::read(elem_ref);
        let elem = func(elem);
        ptr::write(elem_ref, elem);
    }
}

/// Mutate a referenced element by transferring ownership through a function, which also
/// produces an output data which is returned from this function.
fn replace_and_get<T, O>(elem: &mut T, func: impl FnOnce(T) -> (T, O)) -> O {
    unsafe {
        let elem_ref = elem;
        let elem = ptr::read(elem_ref);
        let (elem, out) = func(elem);
        ptr::write(elem_ref, elem);
        out
    }
}

/// A coord which identifies an octant in 3D space.
///
/// The coord has three integer components, as well as a scale.
/// The scale determines the number of levels the identifying octant is above maximum resolution.
///
/// The range covered by an octant identified by this coord in dimension N ranges
/// from `components[N] * 2 ^ scale` (inclusive) to `(components[N] + 1) * 2 ^ scale` (exclusive).
///
/// Currently, the coord is hard-coded to use 64-bit coordinates and three dimensions. Genericity
/// with regard to these properties is a prime candidite for a future feaature.
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct OctCoord {
    base: BaseCoord,
    scale: u8
}
impl OctCoord {
    /// Given a base coord, determine which suboctant value denotes the child of this oct coord
    /// which the base coord would fit into, or none if the coord is not a valid child of this
    /// coord.
    fn suboctant(self, coord: BaseCoord) -> Option<SubOctant> {
        // for it to be a valid child, the bits past scale magnitude must be equal
        if
            childnt(self.base.comps[0], self.scale, coord.comps[0]) ||
            childnt(self.base.comps[1], self.scale, coord.comps[1]) ||
            childnt(self.base.comps[2], self.scale, coord.comps[2]) {
            None
        } else {
            // now that we're sure it is a valid child, we compute each pole base on the greatest
            // uncommon bit
            let x = Pole::from_bit((coord.comps[0] >> (self.scale - 1)) & 0x1).unwrap();
            let y = Pole::from_bit((coord.comps[1] >> (self.scale - 1)) & 0x1).unwrap();
            let z = Pole::from_bit((coord.comps[2] >> (self.scale - 1)) & 0x1).unwrap();
            Some([x, y, z])
        }
    }

    fn to_base(self) -> BaseCoord {
        let x = self.base.comps[0] << self.scale;
        let y = self.base.comps[1] << self.scale;
        let z = self.base.comps[2] << self.scale;
        BaseCoord {
            comps: [x, y, z]
        }
    }

    fn closest_suboctant(self, coord: BaseCoord) -> SubOctant {
        let x = closest_pole(self.base.comps[0], self.scale, coord.comps[0]);
        let y = closest_pole(self.base.comps[1], self.scale, coord.comps[1]);
        let z = closest_pole(self.base.comps[2], self.scale, coord.comps[2]);
        [x, y, z]
    }
}
impl Debug for OctCoord {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.write_str(&format!("{:?}*2^{}", self.base, self.scale))
    }
}

/// Helper function for OctCoord::closest_suboctant
fn closest_pole(oct_comp: u64, oct_scale: u8, base_comp: u64) -> Pole {
    // search for the determinant bit, going from big-endian to little-endian, but stopping at the
    // bit with an endianness of oct_scale
    let max_bit = 0x1 << 63;
    let mut oct_comp = oct_comp << oct_scale;
    let mut base_comp = base_comp;
    for _ in 0..min(64 - oct_scale, 63) {
        if (oct_comp & max_bit) != (base_comp & max_bit) {
            return Pole::from_bit((base_comp & max_bit) >> 63).unwrap();
        }
        oct_comp <<= 1;
        base_comp <<= 1;
    }
    Pole::from_bit((base_comp & max_bit) >> 63).unwrap()
}

/// Helper function for OctCoord::suboctant_base.
fn childnt(scaled_comp: u64, scale: u8, base_comp: u64) -> bool {
    (base_comp >> scale) != scaled_comp
}

/// The component of the coord which is not scaled. When this type is used on its own, it
/// generally refers to a coord with a scale factor of 1.
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct BaseCoord {
    comps: [u64; 3]
}
impl BaseCoord {
    fn lowest_common_octant(self, other: BaseCoord) -> OctCoord {
        // take advantage of the binary representation of the unsigned integers by simply cutting
        // off little-endian bits of the component until they are the same

        // apply that bitwise computation
        let (x_comp, x_scale) = commonize_component(self.comps[0], other.comps[0]);
        let (y_comp, y_scale) = commonize_component(self.comps[1], other.comps[1]);
        let (z_comp, z_scale) = commonize_component(self.comps[2], other.comps[2]);

        // find the minimum scale, which will become to oct coord scale
        let scale = max(max(x_scale, y_scale), z_scale);

        // increase the magnitude of the components to maintain their equality after transitioning
        // them to the possibly smaller scale
        let x = x_comp >> (scale - x_scale);
        let y = y_comp >> (scale - y_scale);
        let z = z_comp >> (scale - z_scale);

        // done
        OctCoord {
            base: BaseCoord {
                comps: [x, y, z],
            },
            scale
        }
    }

    fn manhattan_dist(self, other: BaseCoord) -> u64 {
        abs(self.comps[0] as i64 - other.comps[0] as i64) as u64 +
            abs(self.comps[1] as i64 - other.comps[1] as i64) as u64 +
            abs(self.comps[2] as i64 - other.comps[2] as i64) as u64
    }
}
impl Debug for BaseCoord {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.write_str(&format!("[{},{},{}]", self.comps[0], self.comps[1], self.comps[2]))
    }
}
impl From<[u64; 3]> for BaseCoord {
    fn from(comps: [u64; 3]) -> Self {
        BaseCoord {
            comps
        }
    }
}
impl Into<[u64; 3]> for BaseCoord {
    fn into(self) -> [u64; 3] {
        self.comps
    }
}

/// Helper function for BaseCoord::lowest_common_octant.
fn commonize_component(mut a: u64, mut b: u64) -> (u64, u8) {
    let mut scale = 0;
    while a != b {
        scale += 1;
        a >>= 1;
        b >>= 1;
    }
    (a, scale)
}

/// Positive and negative.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Pole {
    P,
    N,
}
impl Pole {
    #[allow(dead_code)]
    fn from_delta<I: Integer>(delta: I) -> Option<Pole> {
        if delta > I::zero() {
            Some(Pole::P)
        } else if delta < I::zero() {
            Some(Pole::N)
        } else {
            None
        }
    }

    fn from_bit<I: Integer>(bit: I) -> Option<Pole> {
        if bit == I::zero() {
            Some(Pole::N)
        } else if bit == I::one() {
            Some(Pole::P)
        } else {
            None
        }
    }
}
impl Not for Pole {
    type Output = Pole;

    fn not(self) -> <Self as Not>::Output {
        match self {
            Pole::P => Pole::N,
            Pole::N => Pole::P,
        }
    }
}

/// A vector of 3 polarities identified a suboctant.
pub type SubOctant = [Pole; 3];

/// A group of 8 items, identified by suboctant.
#[derive(Debug)]
pub struct Children<T> {
    ppp: T,
    npp: T,
    pnp: T,
    ppn: T,
    pnn: T,
    npn: T,
    nnp: T,
    nnn: T
}
impl<T> Children<T> {
    fn new(mut gen: impl FnMut(SubOctant) -> T) -> Self {
        Children {
            ppp: gen([Pole::P, Pole::P, Pole::P]),
            npp: gen([Pole::N, Pole::P, Pole::P]),
            pnp: gen([Pole::P, Pole::N, Pole::P]),
            ppn: gen([Pole::P, Pole::P, Pole::N]),
            pnn: gen([Pole::P, Pole::N, Pole::N]),
            npn: gen([Pole::N, Pole::P, Pole::N]),
            nnp: gen([Pole::N, Pole::N, Pole::P]),
            nnn: gen([Pole::N, Pole::N, Pole::N]),
        }
    }

    #[allow(dead_code)]
    fn get(&self, key: SubOctant) -> &T {
        match key {
            [Pole::P, Pole::P, Pole::P] => &self.ppp,
            [Pole::N, Pole::P, Pole::P] => &self.npp,
            [Pole::P, Pole::N, Pole::P] => &self.pnp,
            [Pole::P, Pole::P, Pole::N] => &self.ppn,
            [Pole::P, Pole::N, Pole::N] => &self.pnn,
            [Pole::N, Pole::P, Pole::N] => &self.npn,
            [Pole::N, Pole::N, Pole::P] => &self.nnp,
            [Pole::N, Pole::N, Pole::N] => &self.nnn,
        }
    }

    fn get_mut(&mut self, key: SubOctant) -> &mut T {
        match key {
            [Pole::P, Pole::P, Pole::P] => &mut self.ppp,
            [Pole::N, Pole::P, Pole::P] => &mut self.npp,
            [Pole::P, Pole::N, Pole::P] => &mut self.pnp,
            [Pole::P, Pole::P, Pole::N] => &mut self.ppn,
            [Pole::P, Pole::N, Pole::N] => &mut self.pnn,
            [Pole::N, Pole::P, Pole::N] => &mut self.npn,
            [Pole::N, Pole::N, Pole::P] => &mut self.nnp,
            [Pole::N, Pole::N, Pole::N] => &mut self.nnn,
        }
    }
}

fn suboct_search_from(start: Option<SubOctant>, include_start: bool, mut func: impl FnMut(SubOctant)) {
    if let Some(start) = start {
        if include_start {
            func(start);
        }

        // start by flipping a single pole at a time, to touch adjacent tiles
        func([!start[0], start[1], start[2]]);
        func([start[0], !start[1], start[2]]);
        func([start[0], start[1], !start[2]]);

        // then flip 2 poles at a time, to achieve touch tiles
        func([start[0], !start[1], !start[2]]);
        func([!start[0], start[1], !start[2]]);
        func([!start[0], !start[1], start[2]]);

        // then flip att poles for the opposite corner
        func([!start[0], !start[1], !start[2]]);
    } else {
        // if there is no start point, simply searching every combination
        for &x in [Pole::N, Pole::P].iter() {
            for &y in [Pole::N, Pole::P].iter() {
                for &z in [Pole::N, Pole::P].iter() {
                    func([x, y, z]);
                }
            }
        }
    }
}

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

    fn clean(&mut self) {
        // clean up nodes that have become temp with a series of swap_remove
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

    pub fn add(&mut self, coord: impl Into<BaseCoord>, elem: T) {
        let coord = coord.into();
        let (new_root, _) = Octant::add(None, self.root, self, coord, elem);
        self.root = Some(new_root);
        self.clean();
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

struct OctDebug<'a, T: Debug> {
    nodes: &'a [Octant<T>],
    index: Option<usize>,
}
impl<'a, T: Debug> Debug for OctDebug<'a, T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        if let Some(index) = self.index {
            match &self.nodes[index] {
                &Octant::Leaf {
                    coord: ref coord,
                    elems: ref elems,
                    ..
                } => {
                    f.debug_struct("Leaf")
                        .field("coord", coord)
                        .field("elems", elems)
                        .finish()
                },
                &Octant::Branch {
                    coord: ref coord,
                    children: ref children,
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

#[derive(Debug)]
enum Octant<T> {
    Leaf {
        coord: BaseCoord,
        elems: SmallQueue<T>,
        parent: Option<usize>,
    },
    Branch {
        coord: OctCoord,
        children: Children<Option<usize>>,
        bounds: CompBounds<u64>,
        parent: Option<usize>,
    },
    Temp,
}
impl<T> Octant<T> {
    fn leaf_of(parent: Option<usize>, coord: BaseCoord, elem: T) -> Self {
        Octant::Leaf {
            coord,
            elems: SmallQueue::of(elem),
            parent,
        }
    }

    fn add(this_parent: Option<usize>, this_i: Option<usize>, tree: &mut Tree<T>, elem_coord: BaseCoord, elem: T) -> (usize, bool) {
        let (this, this_i) = match this_i {
            Some(i) => (Some(mem::replace(&mut tree.nodes[i], Octant::Temp)), i),
            None => (None, tree.add_node(Octant::Temp))
        };
        let (this, created) = match this {
            None => {
                (Octant::leaf_of(this_parent, elem_coord, elem), true)
            },
            Some(Octant::Leaf {
                coord: leaf_coord,
                elems: mut leaf_elems,
                parent: _,
            }) => {
                if leaf_coord == elem_coord {
                    leaf_elems.add(elem);
                    (Octant::Leaf {
                        parent: this_parent,
                        coord: leaf_coord,
                        elems: leaf_elems,
                    }, false)
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
                    let new_child = Octant::leaf_of(Some(this_i), elem_coord, elem);
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

                    let mut bounds = CompBounds::new();
                    bounds.add(elem_coord);
                    bounds.add(leaf_coord);

                    (Octant::Branch {
                        coord: branch_coord,
                        children,
                        bounds,
                        parent: this_parent,
                    }, true)
                }
            },
            Some(Octant::Branch {
                coord: branch_coord,
                children: mut branch_children,
                mut bounds,
                parent: _,
            }) => {
                if let Some(child_suboctant) = branch_coord.suboctant(elem_coord) {
                    let (new_child, created) =
                        Octant::add(Some(this_i), *branch_children.get(child_suboctant), tree, elem_coord, elem);
                    *branch_children.get_mut(child_suboctant) = Some(new_child);
                    if created {
                        bounds.add(elem_coord);
                    }
                    (Octant::Branch {
                        coord: branch_coord,
                        children: branch_children,
                        bounds,
                        parent: this_parent,
                    }, created)
                } else {
                    let old_branch_coord = branch_coord;
                    let new_branch_coord = old_branch_coord.to_base()
                        .lowest_common_octant(elem_coord);

                    let mut new_branch_bounds = bounds.clone(); // TODO: this is damn expensive
                    new_branch_bounds.add(elem_coord);

                    let old_suboctant =
                        new_branch_coord.suboctant(old_branch_coord.to_base()).unwrap();
                    let old_child = Octant::Branch {
                        coord: old_branch_coord,
                        children: branch_children,
                        bounds,
                        parent: Some(this_i),
                    };
                    let old_child = tree.add_node(old_child);

                    let new_suboctant = new_branch_coord.suboctant(elem_coord).unwrap();
                    let new_child = Octant::leaf_of(Some(this_i), elem_coord, elem);
                    let new_child = tree.add_node(new_child);

                    let children = Children::new(|suboctant| {
                        if suboctant == old_suboctant {
                            Some(old_child)
                        } else if (suboctant == new_suboctant) {
                            Some(new_child)
                        } else {
                            None
                        }
                    });

                    (Octant::Branch {
                        coord: new_branch_coord,
                        children,
                        bounds: new_branch_bounds,
                        parent: this_parent,
                    }, true)
                }
            },
            Some(Octant::Temp) => unreachable!()
        };

        mem::replace(&mut tree.nodes[this_i], this);
        (this_i, created)
    }


}

#[cfg(not(test))]
extern crate rand;
extern crate stopwatch;

use stopwatch::Stopwatch;

use rand::prng::XorShiftRng;
use rand::{Rng, SeedableRng};

fn main() {
    let mut tree: Tree<i32> = Tree::new();

    for x in 0..4 {
        for y in 0..4 {
            for z in 0..4 {
                tree.add([x, y, z], ((x << 4) | (y << 2) | z) as i32);
            }
        }
    }

    println!("{:#?}", tree);

    /*
    let mut tree = Tree::new();
    //let mut elems = Vec::new();

    let mut rng: XorShiftRng = SeedableRng::from_seed(
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]);

    let mut timer = Stopwatch::start_new();

    for i in 0..100000 {
        let elem = [rng.gen::<u64>() / 8, rng.gen::<u64>() / 8, rng.gen::<u64>() / 8];
        tree.add(elem, ());
        if i % 1000 == 0 {
            println!("inserting element i={}", i);
        }
        //elems.push(elem);
    }

    println!("inserted in {}s", timer.elapsed_ms() as f64 / 1000.0);
    timer.restart();

    let mut rng: XorShiftRng = SeedableRng::from_seed(
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]);

    for i in 0..100000 {
        let focus = [rng.gen::<u64>() / 8, rng.gen::<u64>() / 8, rng.gen::<u64>() / 8];


        let tree_closest: [u64; 3] = tree.closest(focus).unwrap().into();

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
            println!("correct! (i={}, focus={:?})", i, focus);
        }
        */
        if i % 1000 == 0 {
            println!("queried! i={}", i);
        }
    }

    println!("queried in {}s", timer.elapsed_ms() as f64 / 1000.0);

    println!("done!");
    */
}
