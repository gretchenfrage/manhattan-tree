
use bonzai::*;
use tree::*;
use space::CoordSpace;

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

    pub fn get_closest(&self, focus: S::Coord) -> Option<(S::Coord, &T)> {
        unimplemented!()
    }

    pub fn get_closest_mut(&mut self, focus: S::Coord) -> Option<(S::Coord, &mut T)> {
        unimplemented!()
    }

    pub fn remove(&mut self, key: S::Coord) -> Option<T> {
        unimplemented!()
    }

    pub fn remove_closest(&mut self, focus: S::Coord) -> Option<(S::Coord, T)> {
        unimplemented!()
    }

    pub fn is_empty(&self) -> bool {
        unimplemented!()
    }
}