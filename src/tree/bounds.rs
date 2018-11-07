
use std::u64;

#[derive(Copy, Clone, Debug)]
pub struct CompBounds {
    pub min: [u64; 3],
    pub max: [u64; 3],
}
impl CompBounds {
    pub fn of(comps: impl Into<[u64; 3]>) -> Self {
        let comps = comps.into();
        CompBounds {
            min: comps,
            max: comps,
        }
    }

    pub fn combine(bounds: &[CompBounds]) -> Self {
        let mut min = [u64::MAX; 3];
        let mut max = [u64::MIN; 3];
        for &CompBounds {
            min: ref elem_min,
            max: ref elem_max,
        } in bounds.iter() {
            for i in 0..3 {
                if elem_min[i] < min[i] {
                    min[i] = elem_min[i];
                }
                if elem_max[i] > max[i] {
                    max[i] = elem_max[i];
                }
            }
        }
        CompBounds {
            min,
            max,
        }
    }
}
