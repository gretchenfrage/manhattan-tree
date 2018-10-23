
use std::ops::Not;

use num::Integer;

/// Positive and negative.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Pole {
    P,
    N,
}
impl Pole {
    #[allow(dead_code)]
    pub fn from_delta<I: Integer>(delta: I) -> Option<Pole> {
        if delta > I::zero() {
            Some(Pole::P)
        } else if delta < I::zero() {
            Some(Pole::N)
        } else {
            None
        }
    }

    pub fn from_bit<I: Integer>(bit: I) -> Option<Pole> {
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
    pub fn new(mut gen: impl FnMut(SubOctant) -> T) -> Self {
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
    pub fn get(&self, key: SubOctant) -> &T {
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

    pub fn get_mut(&mut self, key: SubOctant) -> &mut T {
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

pub fn suboct_search_from(start: Option<SubOctant>, include_start: bool, mut func: impl FnMut(SubOctant)) {
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