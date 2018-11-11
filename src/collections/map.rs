
use bonzai::*;
use tree::*;
use space::CoordSpace;

/// A manhattan tree key/value mapping, where the keys are coordinates in some coordinate space.
///
/// Allows the usual map operations, as well as getting, getting mutably, and removing the closest
/// element in the tree to a particular focus.
pub struct MTreeMap<T, S: CoordSpace> {
    tree: MTree<T>,
    space: S,
}
impl<T, S: CoordSpace> MTreeMap<T, S> {
    pub fn new(space: S) -> Self {
        MTreeMap {
            tree: MTree::new(),
            space,
        }
    }

    pub fn get(&self, key: S::Coord) -> Option<&T> {
        MTree::get(&self.tree, self.space.raw(key))
            .map(|guard| match guard.elem {
                &Octant::Leaf {
                    ref elem,
                    ..
                } => elem,
                &Octant::Branch { .. } => unreachable!()
            })
    }

    pub fn get_mut(&mut self, key: S::Coord) -> Option<&mut T> {
        if let Some(node) = MTree::get(&self.tree, self.space.raw(key)) {
            let octant = get_elem_mut!(self.tree, node);
            let elem = match octant {
                &mut Octant::Leaf {
                    ref mut elem,
                    ..
                } => elem,
                &mut Octant::Branch { .. } => unreachable!()
            };
            Some(elem)
        } else {
            None
        }
    }

    pub fn get_closest(&self, focus: S::Coord) -> Option<&T> {
        MTree::get_closest(&self.tree, self.space.raw(focus))
            .map(|guard| match guard.elem {
                &Octant::Leaf {
                    ref elem,
                    ..
                } => elem,
                &Octant::Branch { .. } => unreachable!()
            })
    }

    pub fn get_closest_mut(&mut self, focus: S::Coord) -> Option<&mut T> {
        if let Some(node) = MTree::get_closest(&self.tree, self.space.raw(focus)) {
            let octant = get_elem_mut!(self.tree, node);
            let elem = match octant {
                &mut Octant::Leaf {
                    ref mut elem,
                    ..
                } => elem,
                &mut Octant::Branch { .. } => unreachable!()
            };
            Some(elem)
        } else {
            None
        }
    }

    pub fn insert(&mut self, key: S::Coord, value: T) {
        self.tree.upsert(self.space.raw(key), InsertUpserter(value))
    }

    pub fn remove(&mut self, key: S::Coord) -> Option<T> {
        let mut op = self.tree.operation();
        if let Some(node) = MTree::get(&op, self.space.raw(key)) {
            Some(MTree::remove(traverse_from!(op, node)))
        } else {
            None
        }
    }

    pub fn remove_closest(&mut self, focus: S::Coord) -> Option<T> {
        let mut op = self.tree.operation();
        if let Some(node) = MTree::get_closest(&op, self.space.raw(focus)) {
            Some(MTree::remove(traverse_from!(op, node)))
        } else {
            None
        }
    }

    pub fn is_empty(&self) -> bool {
        self.tree.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        (&self.tree).into_iter()
    }
}

struct InsertUpserter<T>(T);
impl<T> Upserter<T> for InsertUpserter<T> {
    fn update(self, elem: &mut T) {
        let InsertUpserter(value) = self;
        *elem = value;
    }

    fn insert(self) -> T {
        let InsertUpserter(value) = self;
        value
    }
}