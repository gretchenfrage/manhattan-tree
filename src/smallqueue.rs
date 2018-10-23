
use transform::{replace, replace_and_get};

use std::collections::VecDeque;
use std::fmt::{Debug, Formatter};
use std::fmt;

/// A FIFO queue which stores its data in-place when containing 0 or 1 elements, but
/// expands to a dynamically sized heap allocation when more elements are inserted, and
/// can free its heap allocation if the size returns later to 1 or 0.
pub struct SmallQueue<T> {
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