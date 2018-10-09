
#![feature(nll)]

extern crate num;
extern crate take_mut;

#[cfg(test)]
mod test;
mod bounds;

use std::collections::VecDeque;
use std::cmp::max;
use std::fmt::{Debug, Formatter};
use std::fmt;

use take_mut::take;
use num::Integer;

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
}
impl Debug for OctCoord {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.write_str(&format!("{:?}*2^{}", self.base, self.scale))
    }
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

/// Ahe manhattan tree.
#[derive(Debug)]
pub struct Tree<T> {
    root: Option<Octant<T>>
}
impl<T> Tree<T> {
    pub fn new() -> Self {
        Tree {
            root: None
        }
    }

    pub fn add(&mut self, coord: impl Into<BaseCoord>, elem: T) {
        let coord = coord.into();
        if let Some(ref mut root) = self.root {
            root.add(coord, elem);
        } else {
            self.root = Some(Octant::leaf_of(coord, elem));
        }
    }

    pub fn take(&mut self, focus: impl Into<BaseCoord>) -> Option<T> {
        let focus = focus.into();
        let (elem, emptied) =
            if let Some(ref mut root) = self.root {
                let (elem, emptied) = root.take(focus);
                (Some(elem), emptied)
            } else {
                (None, false)
            };
        if emptied {
            self.root = None;
        }
        elem
    }

    pub fn is_empty(&self) -> bool {
        self.root.is_none()
    }
}

/// An octant in the manhattan tree.
#[derive(Debug)]
enum Octant<T> {
    Leaf {
        coord: BaseCoord,
        elems: VecDeque<T>, // TODO: store in-place, with genericity over array size
    },
    Branch {
        coord: OctCoord,
        children: Children<Box<Octant<T>>>,
    },
    Empty,
}
impl<T> Octant<T> {
    /// Create a leaf octant which contains a single element
    fn leaf_of(coord: BaseCoord, elem: T) -> Self {
        let mut elems = VecDeque::new();
        elems.push_back(elem);
        Octant::Leaf {
            coord,
            elems
        }
    }

    /// Does not assume that the coordinate is part of this octant's range
    fn add(&mut self, elem_coord: BaseCoord, elem: T) {
        // replace self through pattern matching
        take(self, |octant| match octant {
            Octant::Leaf {
                coord: leaf_coord,
                elems: mut leaf_elems,
            } => {
                // case 1: we're a leaf
                if leaf_coord == elem_coord {
                    // case 1a: we've found matching coords
                    // simply insert into queue
                    leaf_elems.push_back(elem);
                    Octant::Leaf {
                        coord: leaf_coord,
                        elems: leaf_elems
                    }
                } else {
                    // case 1b: the coords are non-identical
                    // branch at the smallest common octant
                    let branch_coord = elem_coord.lowest_common_octant(leaf_coord);

                    println!("branching leaves {:?} and {:?} at {:?}", elem_coord, leaf_coord, branch_coord);

                    let old_suboctant = branch_coord.suboctant(leaf_coord).unwrap();
                    let old_child = Octant::Leaf {
                        coord: leaf_coord,
                        elems: leaf_elems
                    };
                    let mut old_child = Some(old_child);

                    let new_suboctant = branch_coord.suboctant(elem_coord).unwrap();
                    let new_child = Octant::leaf_of(elem_coord, elem);
                    let mut new_child = Some(new_child);

                    let children = Children::new(|suboctant| {
                        if suboctant == old_suboctant {
                            Box::new(old_child.take().unwrap())
                        } else if suboctant == new_suboctant {
                            Box::new(new_child.take().unwrap())
                        } else {
                            Box::new(Octant::Empty)
                        }
                    });

                    Octant::Branch {
                        coord: branch_coord,
                        children
                    }
                }
            },
            Octant::Branch {
                coord: branch_coord,
                children: mut branch_children,
            } => {
                // case 2: we're a branch
                if let Some(child) = branch_coord.suboctant(elem_coord) {
                    // case 2a: the new element is a child of this branch
                    // simply add to the appropriate child
                    branch_children.get_mut(child).add(elem_coord, elem);
                    Octant::Branch {
                        coord: branch_coord,
                        children: branch_children,
                    }
                } else {
                    // case 2b: the new element is not a child of this branch
                    // in this case, we need to produce a new super branch

                    let old_branch_coord = branch_coord;
                    let new_branch_coord = old_branch_coord.to_base()
                        .lowest_common_octant(elem_coord);

                    let old_suboctant =
                        new_branch_coord.suboctant(old_branch_coord.to_base()).unwrap();
                    let old_child = Octant::Branch {
                        coord: old_branch_coord,
                        children: branch_children
                    };
                    let mut old_child = Some(old_child);

                    let new_suboctant = new_branch_coord.suboctant(elem_coord).unwrap();
                    let new_child = Octant::leaf_of(elem_coord, elem);
                    let mut new_child = Some(new_child);

                    let children = Children::new(|suboctant| {
                        if suboctant == old_suboctant {
                            Box::new(old_child.take().unwrap())
                        } else if suboctant == new_suboctant {
                            Box::new(new_child.take().unwrap())
                        } else {
                            Box::new(Octant::Empty)
                        }
                    });

                    Octant::Branch {
                        coord: new_branch_coord,
                        children
                    }
                }
            },
            Octant::Empty => {
                // case 3: we're empty
                // simply become a single-element leaf
                Octant::leaf_of(elem_coord, elem)
            }
        })
    }

    /// Take an element, and return whether this octant has become empty, and therefore
    /// invalid and in need of deletion.
    pub fn take(&mut self, _focus: BaseCoord) -> (T, bool) {
        unimplemented!()
    }
}

fn main() {
    let mut tree = Tree::new();

    /*
    tree.add([0, 0, 0], "a");
    tree.add([0, 0, 0], "a2");
    tree.add([1, 1, 1], "b");
    */

    for x in 0..2 {
        for y in 0..3 {
            for z in 0..5 {
                println!("adding {}{}{}", z, y, z);
                tree.add([x, y, z], format!("{}{}{}", x, y, z));
            }
        }
    }

    println!("{:#?}", tree);
}
