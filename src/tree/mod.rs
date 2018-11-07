
mod coord;
mod transform;
mod children;
mod bounds;

pub use self::transform::*;

use self::coord::*;
use self::children::*;
use self::bounds::*;
use self::transform::*;

use std::fmt::Debug;

use bonzai::*;

pub struct MTree<T> {
    tree: Tree<Octant<T>, [ChildId; 8]>
}
impl<T> MTree<T> {
    /// Create a new empty manhattan tree.
    pub fn new() -> Self {
        MTree {
            tree: Tree::new()
        }
    }

    pub fn debug_nodes(&self) -> DebugNodes<Octant<T>, [ChildId; 8]> where T: Debug {
        self.tree.debug_nodes()
    }

    /// Insert a new element at the key, or if an element already exists, mutate it.
    pub fn upsert(&mut self, coord: impl Into<BaseCoord>, upserter: impl Upserter<T>) {
        let coord = coord.into();
        let mut op = self.tree.operation();
        if let Some(root) = op.write_root() {
            let (octant, children) = root.into_split();
            octant.upsert(children, coord, upserter);
        } else {
            op.put_root_elem(Octant::leaf_of(coord, upserter.insert()));
        }
    }

    pub fn operation(&mut self) -> TreeOperation<Octant<T>, [ChildId; 8]> {
        self.tree.operation()
    }

    /// Get a read guard for the element at the key.
    pub fn get<'t>(op: &mut TreeOperation<'t, Octant<T>, [ChildId; 8]>, coord: impl Into<BaseCoord>)
        -> Option<NodeReadGuard<'t, Octant<T>, [ChildId; 8]>> {
        unimplemented!()
    }

    pub fn get_closest<'t: 'o, 'o>(op: &'o TreeOperation<'t, Octant<T>, [ChildId; 8]>,
                                   focus: impl Into<BaseCoord>)
        -> Option<NodeReadGuard<'o, Octant<T>, [ChildId; 8]>> {
        let focus = focus.into();
        if let Some(root) = op.read_root() {
            Some(Octant::closest(root, focus, None).unwrap())
        } else {
            None
        }
    }

    /*
    pub fn remove<'t: 'o, 'o>(mut node: TreeWriteTraverser<'o, 't, Octant<T>, [ChildId; 8]>) -> T {
        let octant = match node.this_branch_index() {
            Ok(leaf_index) => {
                // traverse to the leaf's parent, remove the leaf, acquire the element
                node.seek_parent().unwrap();
                let elem = node.detach_child(leaf_index).unwrap().unwrap().into_elem();

                // if the current branch only has 1 child, we must remove it
                // and reattach the one child to the parent
                let mut child = None;
                let mut num_children = 0;
                for b in 0..8 {
                    if node.has_child(b).unwrap() {
                        child = Some(b);
                        num_children += 1;
                        if num_children >= 2 {
                            break;
                        }
                    }
                }

                match (num_children, child) {
                    (0, _) => unreachable!("old branch only had one child"),
                    (1, Some(only_child_index)) => {
                        // detach the child
                        let only_child = node.detach_child(only_child_index).unwrap().unwrap();
                        // seek the parent
                        match node.this_branch_index() {
                            Ok(branch_index) => {
                                // traverse to the branch's parent
                                node.seek_parent().unwrap();

                                // attach the leaf in replacement with the branch
                                (&mut node).into_write_guard().children().put_child_tree(
                                    branch_index, only_child
                                ).unwrap();
                            },
                            Err(NoParent::Root) => {
                                // this branch which forked into the two leaves was the root
                                // so make the leaf the root in replacement of the branch
                                node.op.put_root_tree(only_child);
                            },
                            Err(NoParent::Detached) => unreachable!()
                        }
                    },
                    (_, None) => unreachable!(),
                    (_, Some(_)) => (),
                };

                elem
            },
            Err(NoParent::Root) => {
                // the leaf we're removing is the root, so no further restructuring is required
                node.detach_this().into_elem()
            },
            Err(NoParent::Detached) => unreachable!()
        };
        match octant {
            Octant::Leaf {
                elem,
                ..
            } => elem,
            Octant::Branch { .. } => unreachable!()
        }
    }
    */

    pub fn remove<'o, 't: 'o>(mut node: TreeWriteTraverser<'o, 't, Octant<T>, [ChildId; 8]>) -> T {
        let octant = match node.this_branch_index() {
            Ok(leaf_index) => {
                // traverse to the leaf's parent, remove the leaf, acquire the element
                node.seek_parent().unwrap();
                let elem = node.detach_child(leaf_index).unwrap().unwrap().into_elem();

                // if the current branch only has 1 child, we must remove it
                // and reattach the one child to the parent
                let mut child = None;
                let mut num_children = 0;
                for b in 0..8 {
                    if node.has_child(b).unwrap() {
                        child = Some(b);
                        num_children += 1;
                        if num_children >= 2 {
                            break;
                        }
                    }
                }

                match (num_children, child) {
                    (0, _) => unreachable!("old branch only had one child"),
                    (1, Some(only_child_index)) => {
                        // detach the child
                        let only_child = node.detach_child(only_child_index).unwrap().unwrap();
                        // seek the parent
                        match node.this_branch_index() {
                            Ok(branch_index) => {
                                // traverse to the branch's parent
                                node.seek_parent().unwrap();

                                // attach the leaf in replacement with the branch
                                (&mut node).into_write_guard().children().put_child_tree(
                                    branch_index, only_child
                                ).unwrap();
                            },
                            Err(NoParent::Root) => {
                                // this branch which forked into the two leaves was the root
                                // so make the leaf the root in replacement of the branch
                                node.op.put_root_tree(only_child);
                            },
                            Err(NoParent::Detached) => unreachable!()
                        }
                    },
                    (_, None) => unreachable!(),
                    (_, Some(_)) => (),
                };

                elem
            },
            Err(NoParent::Root) => {
                // the leaf we're removing is the root, so no further restructuring is required
                node.detach_this().into_elem()
            },
            Err(NoParent::Detached) => unreachable!()
        };
        match octant {
            Octant::Leaf {
                elem,
                ..
            } => elem,
            Octant::Branch { .. } => unreachable!()
        }
    }

    /*
    pub fn remove<'t: 'r, 'r>(op: &mut TreeOperation<'t, Octant<T>, [ChildId; 8]>,
                      node: impl IntoReadGuard<'r, Octant<T>, [ChildId; 8]>) -> T {
        // traverse from the guard
        let mut node = traverse_from!(op, node);

        let octant = match node.this_branch_index() {
            Ok(leaf_index) => {
                // traverse to the leaf's parent, remove the leaf, acquire the element
                node.seek_parent().unwrap();
                let elem = node.detach_child(leaf_index).unwrap().unwrap().into_elem();

                // if the current branch only has 1 child, we must remove it
                // and reattach the one child to the parent
                let mut child = None;
                let mut num_children = 0;
                for b in 0..8 {
                    if node.has_child(b).unwrap() {
                        child = Some(b);
                        num_children += 1;
                        if num_children >= 2 {
                            break;
                        }
                    }
                }

                match (num_children, child) {
                    (0, _) => unreachable!("old branch only had one child"),
                    (1, Some(only_child_index)) => {
                        // detach the child
                        let only_child = node.detach_child(only_child_index).unwrap().unwrap();
                        // seek the parent
                        match node.this_branch_index() {
                            Ok(branch_index) => {
                                // traverse to the branch's parent
                                node.seek_parent().unwrap();

                                // attach the leaf in replacement with the branch
                                (&mut node).into_write_guard().children().put_child_tree(
                                    branch_index, only_child
                                ).unwrap();
                            },
                            Err(NoParent::Root) => {
                                // this branch which forked into the two leaves was the root
                                // so make the leaf the root in replacement of the branch
                                node.op.put_root_tree(only_child);
                            },
                            Err(NoParent::Detached) => unreachable!()
                        }
                    },
                    (_, None) => unreachable!(),
                    (_, Some(_)) => (),
                };

                elem
            },
            Err(NoParent::Root) => {
                // the leaf we're removing is the root, so no further restructuring is required
                node.detach_this().into_elem()
            },
            Err(NoParent::Detached) => unreachable!()
        };
        match octant {
            Octant::Leaf {
                elem,
                ..
            } => elem,
            Octant::Branch { .. } => unreachable!()
        }
    }
    */

    /*
    pub fn get(&self, coord: impl Into<BaseCoord>) -> Option<NodeReadGuard<Octant<T>, [ChildId; 8]>> {
        unimplemented!()
    }
    */


    /*
    /// Get the read guard for the element closest to the key.
    pub fn find_closest(&self, focus: impl Into<BaseCoord>) -> Option<NodeReadGuard<Octant<T>, [ChildId; 8]>> {
        let focus = focus.into();
        self.tree.read_root()
            .map(|root_guard| Octant::closest(root_guard, focus, None)
                .unwrap())
    }

    pub fn find_closest_then<T>(&mut self, focus: impl Into<BaseCoord>,
                                then: impl FnOnce(
                                    &mut TreeOperation<Octant<T>, [ChildId; 8]>,
                                    NodeReadGuard<Octant<T>, [ChildId; 8]>
                                ) -> T) -> Option<T> {
        let mut op = self.tree.operation();
        if let Some(root) = op.read_root() {
            let closest = Octant::closest(root, focus, None).unwrap();
            let output = then(&mut op, closest);
            Some(output)
        } else {
            None
        }
    }

    //pub fn mutate_from_closest<'s: 'op, 'op, O>(&'s mut self, focus: impl Into<BaseCoord>, mutator: impl FnOnce(TreeWriteTraverser<'op, 't, Octant<T>, [ChildId; 8]>) -> O) -> O;

    /// Properly remove the guarded element.
    pub fn remove(&mut self, node: NodeReadGuard<T, [ChildId; 8]>) {
        let mut op = self.tree.operation();
    }
    */

    // TODO: simply find-closest, remove-closest, and remove-key
    // TODO: keep it simple
}

pub trait Upserter<T> {
    fn update(self, elem: &mut T);

    fn insert(self) -> T;
}

#[derive(Debug)]
pub enum Octant<T> {
    Leaf {
        coord: BaseCoord,
        elem: T
    },
    Branch {
        coord: OctCoord,
        bounds: CompBounds,
    },
}
impl<T> Octant<T> {
    fn leaf_of(coord: BaseCoord, elem: T) -> Self {
        Octant::Leaf {
            coord,
            elem
        }
    }

    fn upsert(&mut self, mut children: ChildWriteGuard<Octant<T>, [ChildId; 8]>,
              elem_coord: BaseCoord, upserter: impl Upserter<T>) {
        replace(self, |octant| match octant {
            Octant::Leaf {
                coord: leaf_coord,
                elem: mut leaf_elem,
            } => {
                // case 1: we are a leaf
                if leaf_coord == elem_coord {
                    // case 1a: we've found mathcing coords
                    // simply update the leaf elem
                    upserter.update(&mut leaf_elem);

                    Octant::Leaf {
                        coord: leaf_coord,
                        elem: leaf_elem,
                    }
                } else {
                    // case 1b: the coords are non-identical
                    // branch at the smallest common octant
                    let branch_coord = elem_coord.lowest_common_octant(leaf_coord);

                    let old_suboctant = branch_coord.suboctant(leaf_coord).unwrap();
                    let old_child = Octant::Leaf {
                        coord: leaf_coord,
                        elem: leaf_elem,
                    };

                    let new_suboctant = branch_coord.suboctant(elem_coord).unwrap();
                    let new_child = Octant::leaf_of(elem_coord, upserter.insert());

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
                        child_octant.upsert(subchildren, elem_coord, upserter);
                    } else {
                        children.put_child_elem(child.to_index(),
                                                Octant::leaf_of(elem_coord, upserter.insert()))
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
                    let new_leaf = Octant::leaf_of(elem_coord, upserter.insert());

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

    fn closest<'tree>(this_guard: NodeReadGuard<'tree, Octant<T>, [ChildId; 8]>, focus: BaseCoord,
                      competitor: Option<BaseCoord>) -> Option<NodeReadGuard<'tree, Octant<T>, [ChildId; 8]>> {
        match &*this_guard {
            &Octant::Leaf {
                coord: leaf_coord,
                ..
            } => {
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
            },
            &Octant::Branch {
                coord: branch_coord,
                bounds: branch_bounds,
            } => {
                // case 2: we're a branch
                if let Some(competitor) = competitor {
                    // case 2a: there's a competitor

                    // if there's a competitor, that should mean that this octant isn't in focus
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

                    // suboct search for the best child
                    let mut best: Option<(NodeReadGuard<'tree, Octant<T>, [ChildId; 8]>, BaseCoord)> = None;
                    suboct_search_from(
                        Some(closest_suboct),
                        true,
                        |suboct| {
                            if let Some(better_child) = this_guard.child(suboct.to_index()).unwrap()
                                .and_then(|child_guard| Self::closest(
                                    child_guard,
                                    focus,
                                    Some(best
                                        .as_ref()
                                        .map(|&(_, best_coord)| best_coord)
                                        .unwrap_or(competitor))
                                )) {
                                let better_coord = match &*better_child {
                                    &Octant::Leaf {
                                        coord,
                                        ..
                                    } => coord,
                                    &Octant::Branch {
                                        ..
                                    } => unreachable!("Octant::closest returned branch guard")
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
                    let closest_suboct: SubOctant = branch_coord.closest_suboctant(focus);
                    let mut best: Option<(NodeReadGuard<'tree, Octant<T>, [ChildId; 8]>, BaseCoord)> = None;
                    suboct_search_from(
                        Some(closest_suboct),
                        true,
                        |suboct| {
                            if let Some(better_child) = this_guard.child(suboct.to_index()).unwrap()
                                .and_then(|child_guard| Self::closest(
                                    child_guard,
                                    focus,
                                    best
                                        .as_ref()
                                        .map(|&(_, coord)| coord)
                                )) {
                                let better_coord = match &*better_child {
                                    &Octant::Leaf {
                                        coord,
                                        ..
                                    } => coord,
                                    &Octant::Branch {
                                        ..
                                    } => unreachable!("Octant::closest returned branch guard")
                                };
                                best = Some((better_child, better_coord));
                            }
                        }
                    );

                    // done
                    best.map(|(child, _)| child)
                }
            }
        }
    }
}