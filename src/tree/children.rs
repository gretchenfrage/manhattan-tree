
use std::ops::Not;
use std::ops::Index;

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
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct SubOctant {
    pub poles: [Pole; 3],
}
impl SubOctant {
    pub fn new(poles: [Pole; 3]) -> Self {
        SubOctant {
            poles
        }
    }

    pub fn to_index(self) -> usize {
        match self.poles {
            [Pole::P, Pole::P, Pole::P] => 0,
            [Pole::N, Pole::P, Pole::P] => 1,
            [Pole::P, Pole::N, Pole::P] => 2,
            [Pole::P, Pole::P, Pole::N] => 3,
            [Pole::P, Pole::N, Pole::N] => 4,
            [Pole::N, Pole::P, Pole::N] => 5,
            [Pole::N, Pole::N, Pole::P] => 6,
            [Pole::N, Pole::N, Pole::N] => 7,
        }
    }
}
impl Index<usize> for SubOctant {
    type Output = Pole;

    fn index(&self, i: usize) -> &Pole {
        &self.poles[i]
    }
}

pub fn suboct_search_from(start: Option<SubOctant>, include_start: bool, mut func: impl FnMut(SubOctant)) {
    if let Some(start) = start {
        if include_start {
            func(start);
        }

        // start by flipping a single pole at a time, to touch adjacent tiles
        func(SubOctant::new([!start[0], start[1], start[2]]));
        func(SubOctant::new([start[0], !start[1], start[2]]));
        func(SubOctant::new([start[0], start[1], !start[2]]));

        // then flip 2 poles at a time, to achieve touch tiles
        func(SubOctant::new([start[0], !start[1], !start[2]]));
        func(SubOctant::new([!start[0], start[1], !start[2]]));
        func(SubOctant::new([!start[0], !start[1], start[2]]));

        // then flip att poles for the opposite corner
        func(SubOctant::new([!start[0], !start[1], !start[2]]));
    } else {
        // if there is no start point, simply searching every combination
        for &x in [Pole::N, Pole::P].iter() {
            for &y in [Pole::N, Pole::P].iter() {
                for &z in [Pole::N, Pole::P].iter() {
                    func(SubOctant::new([x, y, z]));
                }
            }
        }
    }
}