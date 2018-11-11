
use std::{u64, i64, u32, i32};
use std::default::Default;

/// Under the hood, the manhattan tree always operates with u64 coordinates. Certain panics
/// are known to occur when the u64 if at high magnitudes.
/// As such, operations on manhattan trees are generic over a coordinate space, which
/// defines a coordinate type, and code for converting that coordinate type into a [u64; 3]
/// in which the 4 highest-endian bits are 0.
pub trait CoordSpace {
    type Coord;

    fn raw(&self, coord: Self::Coord) -> [u64; 3];
}

macro_rules! create_map_space {
    ($name:ident, $comp:ty, $map:expr) => {
        #[derive(Copy, Clone, Debug)]
        pub struct $name;
        impl CoordSpace for $name {
            type Coord = [$comp; 3];

            fn raw(&self, coord: [$comp; 3]) -> [u64; 3] {
                map_coord(coord, $map)
            }
        }
    }
}

/// Coordinate types which have a [0, 0, 0] value.
pub trait ZeroCoord {
    fn zero_coord() -> Self;
}
macro_rules! impl_zero_coord {
    ($int:ty, $zero:expr) => {
        impl ZeroCoord for [$int; 3] {
            fn zero_coord() -> Self {
                [$zero; 3]
            }
        }
    }
}

macro_rules! impl_unit_like_default {
    ($t:ty) => {
        impl Default for $t {
            fn default() -> Self {
                use std::mem;
                unsafe { mem::transmute::<(), $t>(()) }
            }
        }
    }
}

pub fn map_coord<A, B>(a: [A; 3], mut map: impl FnMut(A) -> B) -> [B; 3] {
    let [a0, a1, a2] = a;
    [map(a0), map(a1), map(a2)]
}

const U56_MASK: u64 = 0x0FFFFFFF;
/// u64 coordinate space, which chops off the highest-endian 4 bits.
create_map_space!(U56Space, u64, |c| c & U56_MASK);
impl_zero_coord!(u64, 0);
impl_unit_like_default!(U56Space);

/// u32 coordinate space.
create_map_space!(U32Space, u32, |c| c as u64);
impl_zero_coord!(u32, 0);
impl_unit_like_default!(U32Space);

/// i56 coordinate space, which chops of the highest-endian bit, and allows for negative numbers.
create_map_space!(I56Space, i64, |c| (c as i128 - i64::MIN as i128) as u64 & U56_MASK);
impl_zero_coord!(i64, 0);
impl_unit_like_default!(I56Space);

/// i32 coordinate space, which allows for negative numbers.
create_map_space!(I32Space, i32, |c| (c as i64 - i32::MIN as i64) as u64 & U56_MASK);
impl_zero_coord!(i32, 0);
impl_unit_like_default!(I32Space);

/// f64 coordinate space, which allows for numbers up to a maximum range, before strange
/// behavior occurs.
#[derive(Copy, Clone, Debug)]
pub struct F64Space {
    pub range: f64
}
impl CoordSpace for F64Space {
    type Coord = [f64; 3];

    fn raw(&self, coord: [f64; 3]) -> [u64; 3] {
        map_coord(coord, |c| {
            let scaled = c * ((u64::MAX & U56_MASK) as f64 / self.range);
            (scaled as i128 - i64::MIN as i128) as u64 & U56_MASK
        })
    }
}

/// f32 coordinate space, which allows for numbers up to a maximum range, before strange
/// behavior occurs.
#[derive(Copy, Clone, Debug)]
pub struct F32Space {
    pub range: f32
}
impl CoordSpace for F32Space {
    type Coord = [f32; 3];

    fn raw(&self, coord: [f32; 3]) -> [u64; 3] {
        map_coord(coord, |c| {
            let scaled = c * ((u64::MAX & U56_MASK) as f32 / self.range);
            (scaled as i128 - i32::MIN as i128) as u64 & U56_MASK
        })
    }
}

