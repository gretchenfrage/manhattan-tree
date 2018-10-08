
use std::collections::VecDeque;

/// A coord which identifies an octant in 3D space.
///
/// The coord has three integer components, as well as a scale. The scale his at least 1.
/// The scale determines the number of levels the identifying octant is above maximum resolution.
///
/// The range covered by an octant identified by this coord in dimension N ranges
/// from `components[N] * scale` (inclusive) to `(components[N] + 1) * scale` (exclusive).
///
/// Currently, the coord is hard-coded to use 64-bit coordinates and three dimensions. Genericity
/// with regard to these properties is a prime candidite for a future feaature.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Coord {
    components: [u64; 3],
    scale: u8
}

/// Positive and negative.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Pole {
    P,
    N,
}
impl Pole {
    fn from_delta(delta: u64) -> Option<Pole> {
        if delta > 0 {
            Some(Pole::P)
        } else if delta < 0 {
            Some(Pole::N)
        } else {
            None
        }
    }
}

/// A vector of 3 polarities identified a suboctant.
pub type SubOctant = [Pole; 3];

/// A group of 8 items, identified by suboctant.
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
    fn get(&self, key: SubOctant) -> &T {
        match key {
            [Pole::P, Pole::P, Pole::P] => &self.ppp,
            [Pole::N, Pole::P, Pole::P] => &self.npp,
            [Pole::P, Pole::N, Pole::P] => &self.npn,
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
            [Pole::P, Pole::N, Pole::P] => &mut self.npn,
            [Pole::P, Pole::P, Pole::N] => &mut self.ppn,
            [Pole::P, Pole::N, Pole::N] => &mut self.pnn,
            [Pole::N, Pole::P, Pole::N] => &mut self.npn,
            [Pole::N, Pole::N, Pole::P] => &mut self.nnp,
            [Pole::N, Pole::N, Pole::N] => &mut self.nnn,
        }
    }
}

/// Ahe manhattan tree.
pub struct Tree<C: Comp, T> {
    root: Option<Octant<C, T>>
}
impl<C: Comp, T> Tree<C, T> {
    pub fn new() -> Self {
        Tree {
            root: None
        }
    }

    pub fn add(&mut self, coord: Coord<C>, elem: T) {
        if let Some(ref mut root) = self.root {
            root.add(coord, elem);
        } else {
            let mut elems = VecDeque::new();
            elems.push_back(elem);
            self.root = Some(Octant::Leaf {
                coord: coord,
                elems
            })
        }
    }

    pub fn take(&mut self, focus: Coord<C>) -> Option<T> {
        if let Some(ref mut root) = self.root {
            Some(root.take(focus))
        } else {
            None
        }
    }

    pub fn is_empty(&self) -> bool {
        self.root.is_none()
    }
}

/// An octant in the manhattan tree.
enum Octant<C: Comp, T> {
    Leaf {
        coord: Coord<C>,
        elems: VecDeque<T>, // TODO: store in-place, with genericity over array size
    },
    Branch {
        center: Coord<C>
        children: Children<Box<Octant<C, T>>>
    },
}
impl<C: Comp, T> Octant<C, T> {
    /// Does not assume that the coordinate is part of this octant's range
    pub fn add(&mut self, coord: Coord<C>, elem: T) {
        match self {
            &mut Octant::Leaf {
                coord: ref leaf_coord,
                elems: ref mut leaf_elems,
            } => {
                // case 1: we're a leaf
                if leaf_coord == coord {
                    // case 1A: we've found matching coords
                    // simply insert into queue
                    leaf_elems.push_back(elem)
                } else {

                }
            }
        }
    }

    /// Take an element, and return whether this octant has become empty, and therefore
    /// invalid and in need of deletion.
    pub fn take(&mut self, focus: Coord<C>) -> (T, bool) {
        unimplemented!()
    }
}

fn main() {
    println!("Hello, world!");
}
