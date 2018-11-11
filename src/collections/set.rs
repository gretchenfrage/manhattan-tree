
use super::MTreeMap;
use space::CoordSpace;

/// A degenerate case of the MTreeMap. A set of coordinates in a particular coordinate space
/// that allows for queries on the closest element to a particular focus.
pub struct MTreeSet<S: CoordSpace> {
    map: MTreeMap<S::Coord, S>
}
impl<S: CoordSpace> MTreeSet<S> {
    pub fn new(space: S) -> Self {
        MTreeSet {
            map: MTreeMap::new(space)
        }
    }

    pub fn contains(&self, elem: S::Coord) -> bool {
        self.map.get(elem).is_some()
    }

    pub fn closest(&self, focus: S::Coord) -> Option<&S::Coord> {
        self.map.get_closest(focus)
    }

    pub fn insert(&mut self, elem: S::Coord) where S::Coord: Clone {
        self.map.insert(elem.clone(), elem);
    }

    pub fn remove(&mut self, elem: S::Coord) -> bool {
        self.map.remove(elem).is_some()
    }

    pub fn remove_closest(&mut self, focus: S::Coord) -> Option<S::Coord> {
        self.map.remove_closest(focus)
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &S::Coord> {
        self.map.iter()
    }
}
