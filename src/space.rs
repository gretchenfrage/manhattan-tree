
use std::i64;
use num::Float;

pub trait CoordSpace {
    type Coord;

    fn raw(&self, coord: Self::Coord) -> [u64; 3];
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

pub struct U32Space;
impl CoordSpace for U32Space {
    type Coord = [u32; 3];

    fn raw(&self, coord: [u32; 3]) -> [u64; 3] {
        map_coord(coord,
                  |c| c as u64)
    }
}

pub struct I64Space;
impl CoordSpace for I64Space {
    type Coord = [i64; 3];

    fn raw(&self, coord: [i64; 3]) -> [u64; 3] {
        map_coord(coord,
                  |c| (c as i128 - i64::MIN as i128) as u64 & U63_MASK)
    }
}

pub struct I32Space;
impl CoordSpace for I32Space {
    type Coord = [i32; 3];

    fn raw(&self, coord: [i32; 3]) -> [u64; 3] {
        map_coord(coord,
                  |c| (c as i64 - i64::MIN) as u64 & U63_MASK)
    }
}

// TODO: float space