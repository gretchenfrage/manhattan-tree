
use tree::*;
use space::CoordSpace;

use bonzai::traverse_from;
use smallqueue::SmallQueue;

/// A priority queue based on the manhattan tree, where each element is associated with a coord
/// in a particular coordinate space, and upon removal, a focus is specified, and the closest
/// element to that focus is removed.
///
/// When there are multiple elements inserted to the same key, they form a FIFO queue. This queue
/// has a small-queue optimization such that it only creates a heap allocation when more
/// than one element is allocated.
pub struct MTreeQueue<T, S: CoordSpace> {
    space: S,
    tree: MTree<SmallQueue<T>>
}
impl<T, S: CoordSpace> MTreeQueue<T, S> {
    /// New, empty MTreeQueue.
    pub fn new(space: S) -> Self {
        MTreeQueue {
            space,
            tree: MTree::new()
        }
    }

    /// Insert an element at a coordinate.
    pub fn insert(&mut self, coord: S::Coord, elem: T) {
        self.tree.upsert(self.space.raw(coord), SmallQueueUpserter {
            elem
        });
    }

    /// Is the queue empty?
    pub fn is_empty(&self) -> bool {
        self.tree.is_empty()
    }

    /// Remove an element at the key closest to the focus.
    pub fn remove(&mut self, focus: S::Coord) -> Option<T> {
        let mut op = self.tree.operation();
        if let Some(closest) = MTree::get_closest(&op, self.space.raw(focus)) {
            let mut node = traverse_from!(op, closest);
            let (elem, is_empty) = match &mut*node {
                &mut Octant::Leaf {
                    elem: ref mut queue,
                    ..
                } => (queue.remove().unwrap(), queue.is_empty()),
                &mut Octant::Branch { .. } => unreachable!()
            };
            if is_empty {
                MTree::remove(node);
            }
            Some(elem)
        } else {
            None
        }
    }
}

struct SmallQueueUpserter<T> {
    elem: T
}
impl<T> Upserter<SmallQueue<T>> for SmallQueueUpserter<T> {
    fn update(self, queue: &mut SmallQueue<T>) {
        queue.add(self.elem);
    }

    fn insert(self) -> SmallQueue<T> {
        SmallQueue::of(self.elem)
    }
}
