#![feature(nll)]
#![feature(vec_remove_item)]
#![feature(duration_as_u128)]

extern crate num;
extern crate bonzai;

pub mod tree;
pub mod collection;
pub mod space;

#[cfg(test)]
mod test;

/*
use smallqueue::SmallQueue;
use coord::*;
use children::*;
use bounds::*;
use transform::*;

use std::fmt::Debug;

use bonzai::*;

#[derive(Debug)]
pub struct Octree<T> {
    tree: Tree<Octant<T>, [ChildId; 8]>
}
impl<T: 'static> Octree<T> {
    pub fn new() -> Self {
        Octree {
            tree: Tree::new()
        }
    }

    pub fn debug_nodes(&self) -> String where T: Debug {
        format!("{:#?}", self.tree.debug_nodes())
    }

    pub fn add(&mut self, coord: impl Into<BaseCoord>, elem: T) {
        let coord = coord.into();
        let mut op = self.tree.operation();
        if let Some(root) = op.write_root() {
            let (octant, children) = root.into_split();
            octant.add(children, coord, elem);
        } else {
            op.put_root_elem(Octant::leaf_of(coord, elem));
        }
    }

    pub fn is_empty(&self) -> bool {
        self.tree.read_root().is_none()
    }

    pub fn closest_key(&mut self, focus: impl Into<BaseCoord>) -> Option<[u64; 3]> {
        let focus = focus.into();
        let mut op = self.tree.operation();
        if let Some(root) = op.write_root() {
            match Octant::find_closest(root, focus, None).unwrap().elem() {
                &mut Octant::Leaf {
                    coord,
                    ..
                } => Some(coord.comps),
                &mut Octant::Branch {
                    ..
                } => unreachable!()
            }
        } else {
            None
        }
    }

    pub fn remove_closest(&mut self, focus: impl Into<BaseCoord>) -> Option<T> {
        let focus = focus.into();
        let mut op = self.tree.operation();
        let elem = if let Some(root) = op.write_root() {
            // if root exists, there must be an element

            // find the closest element in the form of a guard
            let mut closest = Octant::find_closest(root, focus, None).unwrap();

            // remove an elem from the queue
            let (elem, remove_node) = {
                if let &mut Octant::Leaf {
                    elems: ref mut queue,
                    ..
                } = closest.elem() {
                    let elem = queue.remove().unwrap();
                    (elem, queue.is_empty())
                } else {
                    unreachable!()
                }
            };

            // and if the leaf has become empty, we must remove the node
            if remove_node {

                // turn it into a traverser
                let mut node = traverse_from!(op, closest);

                match node.this_branch_index() {
                    Ok(leaf_index) => {
                        // traverse to the leaf's parent, remove the leaf
                        node.seek_parent().unwrap();
                        node.detach_child(leaf_index).unwrap().unwrap();

                        // if the current branch has only 1 child, we must remove it
                        // and detach the one child with the parent
                        let mut child = None;
                        let mut num_children = 0;
                        for b in 0..8 {
                            if node.has_child(b).unwrap() {
                                child = Some(b);
                                num_children += 1;
                            }
                        }


                        match (num_children, child) {
                            (0, _) => unreachable!("branch old had one child"),
                            (1, Some(only_child_index)) => {
                                // detach the child
                                let only_child = node.detach_child(only_child_index).unwrap().unwrap();
                                // seek the parent
                                // TODO: add a `become` operation to node write guard and tree write traverser
                                match node.this_branch_index() {
                                    Ok(branch_index) => {
                                        // traverse to the branch's parent
                                        node.seek_parent().unwrap();

                                        // attach the leaf in replacement of the branch
                                        (&mut node).into_write_guard().children().put_child_tree(branch_index, only_child).unwrap();
                                    },
                                    Err(NoParent::Root) => {
                                        // the branch which forked into the two leaves was the root
                                        // so make the leaf the root in replacement of the branch
                                        node.op.put_root_tree(only_child);
                                    },
                                    Err(NoParent::Detached) => unreachable!()
                                }
                            },
                            (_, None) => unreachable!(),
                            (_, Some(_)) => (),
                        };
                    },
                    Err(NoParent::Root) => {
                        // the leaf we're removing is the root, so no further
                        // restructuring is required
                        node.detach_this();
                    },
                    Err(NoParent::Detached) => unreachable!(),
                };
            }

            // done
            Some(elem)
        } else {
            // no element
            None
        };
        elem
    }
}

#[derive(Debug)]
enum Octant<T> {
    Leaf {
        coord: BaseCoord,
        elems: SmallQueue<T>,
    },
    Branch {
        coord: OctCoord,
        bounds: CompBounds,
    },
}
impl<T: 'static> Octant<T> {
    fn leaf_of(coord: BaseCoord, elem: T) -> Self {
        Octant::Leaf {
            coord,
            elems: SmallQueue::of(elem),
        }
    }

    fn add(&mut self, mut children: ChildWriteGuard<Octant<T>, [ChildId; 8]>, elem_coord: BaseCoord, elem: T) {
        replace(self, |octant| match octant {
            Octant::Leaf {
                coord: leaf_coord,
                elems: mut leaf_elems,
            } => {
                // case 1: we are a leaf
                if leaf_coord == elem_coord {
                    // case 1a: we've found mathcing coords
                    // simply insert into queue
                    leaf_elems.add(elem);

                    Octant::Leaf {
                        coord: leaf_coord,
                        elems: leaf_elems,
                    }
                } else {
                    // case 1b: the coords are non-identical
                    // branch at the smallest common octant
                    let branch_coord = elem_coord.lowest_common_octant(leaf_coord);

                    let old_suboctant = branch_coord.suboctant(leaf_coord).unwrap();
                    let old_child = Octant::Leaf {
                        coord: leaf_coord,
                        elems: leaf_elems,
                    };

                    let new_suboctant = branch_coord.suboctant(elem_coord).unwrap();
                    let new_child = Octant::leaf_of(elem_coord, elem);

                    let branch_bounds = CompBounds::combine(&[
                        CompBounds::of(elem_coord),
                        CompBounds::of(leaf_coord),
                    ]);

                    children.put_child_elem(
                        old_suboctant.to_index(),
                        old_child,
                    ).unwrap();
                    children.put_child_elem(
                        new_suboctant.to_index(),
                        new_child,
                    ).unwrap();

                    Octant::Branch {
                        coord: branch_coord,
                        bounds: branch_bounds,
                    }
                }
            },
            Octant::Branch {
                coord: branch_coord,
                bounds: branch_bounds,
            } => {
                // case 2: we're a branch
                if let Some(child) = branch_coord.suboctant(elem_coord) {
                    // case 2a: the new element is a child of this branch
                    // simply add to the appropriate child, or create a child


                    if let Some(child) = children
                        .borrow_child_write(child.to_index())
                        .unwrap() {
                        let (child_octant, subchildren) = child.into_split();
                        child_octant.add(subchildren, elem_coord, elem);
                    } else {
                        children.put_child_elem(child.to_index(),
                                                Octant::leaf_of(elem_coord, elem))
                            .unwrap();
                    }

                    // the new bounds can only be more extreme than the current bounds
                    let new_bounds = CompBounds::combine(&[
                        branch_bounds,
                        CompBounds::of(elem_coord)
                    ]);

                    Octant::Branch {
                        coord: branch_coord,
                        bounds: new_bounds,
                    }
                } else {
                    // case 2b: the new element is not a child of this branch
                    // in this case, we need to produce a new super branch

                    let old_branch_coord = branch_coord;
                    let new_branch_coord = old_branch_coord.to_base()
                        .lowest_common_octant(elem_coord);

                    // the new bounds can only be more extreme than the current bounds
                    let old_branch_bounds = branch_bounds;
                    let new_branch_bounds = CompBounds::combine(&[
                        old_branch_bounds,
                        CompBounds::of(elem_coord)
                    ]);

                    // create the old branch as a detached node
                    let mut old_branch = children.op.new_detached(Octant::Branch {
                        coord: old_branch_coord,
                        bounds: old_branch_bounds,
                    });

                    // transfer all current children to the detached "old branch" node
                    {
                        let mut old_branch_children = old_branch.children();
                        for branch_index in 0..8 {
                            if let Some(child) = children.take_child(branch_index).unwrap() {
                                old_branch_children.put_child_tree(branch_index, child).unwrap();
                            }
                        }
                    }

                    // the other child of the new branch will be a leaf
                    // which will contain this here elem
                    let new_leaf = Octant::leaf_of(elem_coord, elem);

                    // now, attach the old branch and the new leaf as children of the new branch (self)
                    let old_suboctant = new_branch_coord
                        .suboctant(old_branch_coord.to_base()).unwrap();
                    children.put_child_tree(old_suboctant.to_index(), old_branch).unwrap();

                    let new_suboctant = new_branch_coord
                        .suboctant(elem_coord).unwrap();
                    children.put_child_elem(new_suboctant.to_index(), new_leaf).unwrap();

                    // and finally, become the new branch
                    Octant::Branch {
                        coord: new_branch_coord,
                        bounds: new_branch_bounds,
                    }
                }
            }
        })
    }

    fn find_closest<'op, 'node, 't>(mut this_guard: NodeWriteGuard<'op, 'node, 't, Octant<T>, [ChildId; 8]>,
                                    focus: BaseCoord, competitor: Option<BaseCoord>)
                                    -> Option<NodeWriteGuard<'op, 'node, 't, Octant<T>, [ChildId; 8]>> {

        if let Some(leaf_coord) = match this_guard.elem() {
            &mut Octant::Leaf {
                coord,
                ..
            } => Some(coord),
            &mut Octant::Branch { .. } => None,
        } {
            // case 1: we're a leaf
            // the closest element is the only element, but it may or may not be better than the
            // competitor manhattan distance
            if let Some(competitor) = competitor {
                if competitor.manhattan_dist(focus) < leaf_coord.manhattan_dist(focus) {
                    None
                } else {
                    Some(this_guard)
                }
            } else {
                Some(this_guard)
            }
        } else {
            // case 2: we're a branch
            let (this_node, children) = this_guard.into_split();
            let closest: Option<NodeWriteGuard<'op, 'node, 't, Octant<T>, [ChildId; 8]>> = if let &mut Octant::Branch {
                coord: branch_coord,
                bounds: branch_bounds,
            } = this_node {
                if let Some(competitor) = competitor {
                    // case 2a: there's a competitor

                    // if there's a competitor, that should only mean that this octant isn't in focus
                    debug_assert!(branch_coord.suboctant(focus).is_none());

                    // short circuit using bounds
                    let closest_suboct = branch_coord.closest_suboctant(focus);
                    for d in 0..3 {
                        if closest_suboct[d] == Pole::N {
                            if (competitor.manhattan_dist(focus) as i64) <
                                branch_bounds.min[d] as i64 - focus.comps[d] as i64 {
                                return None;
                            }
                        } else {
                            if (competitor.manhattan_dist(focus) as i64) <
                                focus.comps[d] as i64 - branch_bounds.max[d] as i64 {
                                return None;
                            }
                        }
                    }

                    // convert the child guard into guards for all its children
                    // TODO: this could be refactored by passing in the branch indices in the correct
                    // TODO: order when converting the child guard into its children guards

                    /*
                    let mut child_guards: [Option<Option<NodeWriteGuard<'op, 'node, 't, Octant<T>, [ChildId; 8]>>>; 8] =
                        Default::default();

                    children.into_all_children_write(|branch, child| {
                        child_guards[branch] = Some(child);
                    });
                    */
                    let mut child_guards: [Option<NodeWriteGuard<'op, 'node, 't, Octant<T>, [ChildId; 8]>>; 8] =
                        Default::default();
                    children.into_all_children(&mut child_guards).unwrap();

                    // suboct search for the best child
                    let mut best: Option<(NodeWriteGuard<'op, 'node, 't, Octant<T>, [ChildId; 8]>, BaseCoord)> = None;
                    suboct_search_from(
                        Some(closest_suboct),
                        true,
                        |suboct| {
                            if let Some(mut better_child) = child_guards[suboct.to_index()]
                                .take()
                                .and_then(|child_guard| Self::find_closest(
                                    child_guard,
                                    focus,
                                    Some(best
                                        .as_ref()
                                        .map(|&(_, best_coord)| best_coord)
                                        .unwrap_or(competitor))
                                )) {
                                let better_coord = match better_child.elem() {
                                    &mut Octant::Leaf {
                                        coord,
                                        ..
                                    } => coord,
                                    &mut Octant::Branch {
                                        ..
                                    } => unreachable!("Octant::find_closest returned branch guard")
                                };
                                best = Some((better_child, better_coord));
                            }
                        }
                    );

                    // done
                    best.map(|(child, _)| child)
                } else {
                    // case 2b: there's no competitor
                    // simply suboct search for the best child from the closest suboct

                    // convert the child guard into guards for all its children
                    /*
                    let mut child_guards: [Option<Option<NodeWriteGuard<'op, 'node, 't, Octant<T>, [ChildId; 8]>>>; 8] =
                        Default::default();

                    children.into_all_children_write(|branch, child| {
                        child_guards[branch] = Some(child);
                    });
                    */
                    let mut child_guards: [Option<NodeWriteGuard<'op, 'node, 't, Octant<T>, [ChildId; 8]>>; 8] =
                        Default::default();
                    children.into_all_children(&mut child_guards).unwrap();

                    // suboct search for the best child
                    let closest_suboct: SubOctant = branch_coord.closest_suboctant(focus);
                    let mut best: Option<(NodeWriteGuard<'op, 'node, 't, Octant<T>, [ChildId; 8]>, BaseCoord)> = None;
                    suboct_search_from(
                        Some(closest_suboct),
                        true,
                        |suboct| {
                            if let Some(mut better_child) = child_guards[suboct.to_index()]
                                .take()
                                .and_then(|child_guard| Self::find_closest(
                                    child_guard,
                                    focus,
                                    best
                                        .as_ref()
                                        .map(|&(_, coord)| coord)
                                )) {
                                let better_coord = match better_child.elem() {
                                    &mut Octant::Leaf {
                                        coord,
                                        ..
                                    } => coord,
                                    &mut Octant::Branch {
                                        ..
                                    } => unreachable!("Octant::find_closest returned branch guard")
                                };
                                best = Some((better_child, better_coord));
                            }
                        }
                    );

                    // done
                    best.map(|(child, _)| child)
                }
            } else {
                unreachable!()
            };
            closest
        }

    }
}
*/