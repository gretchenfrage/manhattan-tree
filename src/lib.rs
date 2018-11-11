#![feature(nll)]
#![feature(vec_remove_item)]
#![feature(duration_as_u128)]

extern crate num;
extern crate bonzai;

pub mod tree;
pub mod collections;
pub mod space;

#[cfg(test)]
mod test;