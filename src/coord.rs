
use children::*;

use std::fmt::{Debug, Formatter};
use std::fmt;
use std::cmp::{min, max};

use num::abs;

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
    pub fn suboctant(self, coord: BaseCoord) -> Option<SubOctant> {
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

    pub fn to_base(self) -> BaseCoord {
        let x = self.base.comps[0] << self.scale;
        let y = self.base.comps[1] << self.scale;
        let z = self.base.comps[2] << self.scale;
        BaseCoord {
            comps: [x, y, z]
        }
    }

    pub fn closest_suboctant(self, coord: BaseCoord) -> SubOctant {
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
    pub comps: [u64; 3]
}
impl BaseCoord {
    pub fn lowest_common_octant(self, other: BaseCoord) -> OctCoord {
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

    pub fn manhattan_dist(self, other: BaseCoord) -> u64 {
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