
use std::collections::{HashSet, BTreeMap};
use std::hash::Hash;
use std::fmt::{Debug, Formatter};
use std::fmt;

use num::Integer;

/// Holds a cloud of vectors, and keeps track of the minimum and maximum value of each component
/// across all the vectors.
#[derive(Clone)]
pub struct CompBounds<I: Integer + Copy + Hash> {
    x: BTreeMap<I, u32>,
    y: BTreeMap<I, u32>,
    z: BTreeMap<I, u32>,
    cloud: HashSet<[I; 3]>,
}
impl<I: Integer + Copy + Hash> CompBounds<I> {
    pub fn new() -> Self {
        CompBounds {
            x: BTreeMap::new(),
            y: BTreeMap::new(),
            z: BTreeMap::new(),
            cloud: HashSet::new(),
        }
    }

    pub fn cloud(&self) -> &HashSet<[I; 3]> {
        &self.cloud
    }

    pub fn min_x(&self) -> Option<I> {
        self.x.iter().next().map(|(&i, _)| i)
    }

    pub fn min_y(&self) -> Option<I> {
        self.y.iter().next().map(|(&i, _)| i)
    }

    pub fn min_z(&self) -> Option<I> {
        self.z.iter().next().map(|(&i, _)| i)
    }

    pub fn max_x(&self) -> Option<I> {
        self.x.iter().next_back().map(|(&i, _)| i)
    }

    pub fn max_y(&self) -> Option<I> {
        self.y.iter().next_back().map(|(&i, _)| i)
    }

    pub fn max_z(&self) -> Option<I> {
        self.z.iter().next_back().map(|(&i, _)| i)
    }

    pub fn add(&mut self, elem: impl Into<[I; 3]>) {
        let elem = elem.into();
        if self.cloud.insert(elem) {
            if let Some(n) = self.x.get_mut(&elem[0]) {
                *n += 1;
            } else {
                self.x.insert(elem[0], 1);
            }

            if let Some(n) = self.y.get_mut(&elem[1]) {
                *n += 1;
            } else {
                self.y.insert(elem[1], 1);
            }

            if let Some(n) = self.z.get_mut(&elem[2]) {
                *n += 1;
            } else {
                self.z.insert(elem[2], 1);
            }
        }
    }

    pub fn remove(&mut self, elem: impl Into<[I; 3]>) {
        let elem = elem.into();
        if self.cloud.remove(&elem) {
            // decrement count, or remove if it would get to 0

            if match self.x.get_mut(&elem[0]) {
                Some(&mut 1) => true,
                Some(n) => {
                    *n -= 1;
                    false
                },
                None => {
                    #[cfg(test)]
                    eprintln!("warning: removing extremes element not present");
                    false
                }
            } {
                self.x.remove(&elem[0]);
            }

            if match self.y.get_mut(&elem[1]) {
                Some(&mut 1) => true,
                Some(n) => {
                    *n -= 1;
                    false
                },
                None => {
                    #[cfg(test)]
                    eprintln!("warning: removing extremes element not present");
                    false
                }
            } {
                self.y.remove(&elem[1]);
            }

            if match self.z.get_mut(&elem[2]) {
                Some(&mut 1) => true,
                Some(n) => {
                    *n -= 1;
                    false
                },
                None => {
                    #[cfg(test)]
                    eprintln!("warning: removing extremes element not present");
                    false
                }
            } {
                self.z.remove(&elem[2]);
            }
        }
    }
}
impl<I: Integer + Copy + Hash + Debug> Debug for CompBounds<I> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        f.write_str(&format!("Bounds({:?})", self.cloud))
    }
}