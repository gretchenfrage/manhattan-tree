
use std::i64;
use std::default::Default;

pub trait CoordSpace {
    type Coord;

    fn raw(&self, coord: Self::Coord) -> [u64; 3];
}

pub trait ZeroCoord {
    fn zero_coord() -> Self;
}
macro_rules! impl_zero_coord_int {
    ($int:ty) => {
        impl ZeroCoord for [$int; 3] {
            fn zero_coord() -> Self {
                [0; 3]
            }
        }
    }
}

pub fn map_coord<A, B>(a: [A; 3], mut map: impl FnMut(A) -> B) -> [B; 3] {
    let [a0, a1, a2] = a;
    [map(a0), map(a1), map(a2)]
}

pub struct U63Space;
const U63_MASK: u64 = !(0x1 << 63);
impl CoordSpace for U63Space {
    type Coord = [u64; 3];

    fn raw(&self, coord: [u64; 3]) -> [u64; 3] {
        map_coord(coord,
                  |c| c & U63_MASK)
    }
}
impl Default for U63Space {
    fn default() -> Self {
        U63Space
    }
}
impl_zero_coord_int!(u64);

pub struct U32Space;
impl CoordSpace for U32Space {
    type Coord = [u32; 3];

    fn raw(&self, coord: [u32; 3]) -> [u64; 3] {
        map_coord(coord,
                  |c| c as u64)
    }
}
impl Default for U32Space {
    fn default() -> Self {
        U32Space
    }
}
impl_zero_coord_int!(u32);

pub struct I63Space;
impl CoordSpace for I63Space {
    type Coord = [i64; 3];

    fn raw(&self, coord: [i64; 3]) -> [u64; 3] {
        map_coord(coord,
                  |c| (c as i128 - i64::MIN as i128) as u64 & U63_MASK)
    }
}
impl Default for I63Space {
    fn default() -> Self {
        I63Space
    }
}
impl_zero_coord_int!(i64);

pub struct I32Space;
impl CoordSpace for I32Space {
    type Coord = [i32; 3];

    fn raw(&self, coord: [i32; 3]) -> [u64; 3] {
        map_coord(coord,
                  |c| (c as i64 - i64::MIN) as u64 & U63_MASK)
    }
}
impl Default for I32Space {
    fn default() -> Self {
        I32Space
    }
}
impl_zero_coord_int!(i32);

// TODO: float space