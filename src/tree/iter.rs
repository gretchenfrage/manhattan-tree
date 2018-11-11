
use super::*;

use std::iter::{IntoIterator, Iterator};
use std::mem;

use bonzai::*;

impl<'t, T> IntoIterator for &'t MTree<T> {
    type Item = &'t T;
    type IntoIter = Iter<'t, T>;

    fn into_iter(self) -> Iter<'t, T> {
        Iter::new(self.traverse_read_root())
    }
}

pub struct Iter<'t, T> {
    trav: Option<TreeReadTraverser<'t, Octant<T>, [ChildId; 8]>>
}
impl<'t, T> Iter<'t, T> {
    fn new(trav: Option<TreeReadTraverser<'t, Octant<T>, [ChildId; 8]>>) -> Self {
        match trav {
            Some(trav) => {
                Self::seek_bottom_left(&trav);
                Iter { trav: Some(trav) }
            },
            None => Iter { trav: None }
        }
    }

    fn seek_bottom_left(trav: &TreeReadTraverser<'t, Octant<T>, [ChildId; 8]>) {
        'seek: loop {
            for b in 0..8 {
                if trav.seek_child(b).unwrap().is_ok() {
                    continue 'seek;
                }
            }
            break 'seek;
        }
    }
}
impl<'t, T> Iterator for Iter<'t, T> {
    type Item = &'t T;

    fn next(&mut self) -> Option<&'t T> {
        match self.trav {
            None => None,
            Some(ref mut trav) => {
                // capture the current element
                let curr = match trav.elem() {
                    &Octant::Leaf {
                        ref elem,
                        ..
                    } => elem,
                    &Octant::Branch { .. } => unreachable!(),
                };

                // move up until we can move down into the next branch index
                'up: loop {
                    match trav.this_branch_index() {
                        Ok(this_branch) =>{
                            trav.seek_parent().unwrap();
                            for next_branch in this_branch + 1..8 {
                                // if we can seek down to the side
                                if trav.seek_child(next_branch).unwrap().is_ok() {
                                    // then seek bottom left and break out
                                    Self::seek_bottom_left(trav);
                                    assert!(match trav.elem() {
                                        &Octant::Leaf { .. } => true,
                                        &Octant::Branch { .. } => false,
                                    });
                                    break 'up;
                                }
                            }
                            // if we can't seek down to the side, then we
                            // only stay here if we're a leaf
                            match trav.elem() {
                                &Octant::Leaf { .. } => {
                                    break 'up;
                                },
                                &Octant::Branch { .. } => {
                                    continue 'up;
                                },
                            };
                        },
                        Err(_) => {
                            // however, if we've hit the top, that means we're done iterating
                            mem::drop(trav);
                            self.trav = None;
                            break 'up;
                        }
                    }
                };

                Some(curr)
            }
        }
    }
}