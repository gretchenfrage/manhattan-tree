#![feature(nll)]
#![feature(vec_remove_item)]
#![feature(duration_as_u128)]

extern crate bonzai;
extern crate num;

pub mod tree;
pub mod collections;
pub mod space;

#[cfg(test)]
mod test;