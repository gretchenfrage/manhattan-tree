
use tree::*;
use space::CoordSpace;

use std::collections::VecDeque;
use std::fmt::{Debug, Formatter};
use std::fmt;

use bonzai::traverse_from;

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

/// A FIFO queue which stores its data in-place when containing 0 or 1 elements, but
/// expands to a dynamically sized heap allocation when more elements are inserted, and
/// can free its heap allocation if the size returns later to 1 or 0.
struct SmallQueue<T> {
    state: SmallQueueState<T>
}
impl<T> SmallQueue<T> {
    #[allow(dead_code)]
    pub fn new() -> Self {
        SmallQueue {
            state: SmallQueueState::Zero
        }
    }

    pub fn of(elem: T) -> Self {
        SmallQueue {
            state: SmallQueueState::One(elem)
        }
    }

    pub fn add(&mut self, elem: T) {
        replace(&mut self.state, move |state| match state {
            SmallQueueState::Zero => SmallQueueState::One(elem),
            SmallQueueState::One(present) => {
                let mut queue = VecDeque::new();
                queue.push_back(present);
                queue.push_back(elem);
                SmallQueueState::Several(queue)
            },
            SmallQueueState::Several(mut queue) => {
                queue.push_back(elem);
                SmallQueueState::Several(queue)
            }
        })
    }

    pub fn remove(&mut self) -> Option<T> {
        replace_and_get(&mut self.state, |state| match state {
            SmallQueueState::Zero => (SmallQueueState::Zero, None),
            SmallQueueState::One(elem) => (SmallQueueState::Zero, Some(elem)),
            SmallQueueState::Several(mut queue) => {
                let elem = queue.pop_front();
                if queue.len() == 1 {
                    (SmallQueueState::One(queue.pop_front().unwrap()), elem)
                } else {
                    (SmallQueueState::Several(queue), elem)
                }
            }
        })
    }

    pub fn is_empty(&self) -> bool {
        match &self.state {
            &SmallQueueState::Zero => true,
            _ => false,
        }
    }
}

enum SmallQueueState<T> {
    Zero,
    One(T),
    Several(VecDeque<T>)
}

impl<T: Debug> Debug for SmallQueue<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        match self.state {
            SmallQueueState::Zero => f.write_str("[]"),
            SmallQueueState::One(ref elem) => {
                f.write_str("[")?;
                elem.fmt(f)?;
                f.write_str("]")
            },
            SmallQueueState::Several(ref queue) => queue.fmt(f),
        }
    }
}