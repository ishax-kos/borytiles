use std::{array, ops::Index, path::Iter};
pub use std::{
	collections::{ 
		BTreeSet, BTreeMap
	}, 
	path::{PathBuf, Path}, 
	str::FromStr
};

pub use crate::bucket::*;

pub const raw_primary_path: &str = "./samples/data/tilesets/primary/";
pub const raw_secondary_path: &str = "./samples/data/tilesets/secondary/";

pub const metatile_behaviors_path: &str = "./samples/include/constants/metatile_behaviors.h";




#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Small_vec<E, const max_size: usize> {
	length: u8,
	data: [E; max_size]
}


impl<E, const max_size: usize> Small_vec<E, max_size> {

	fn get(&self, index: usize) -> Option<&E> {
		if index >= self.length as usize || index >= max_size {return None}
		Some(unsafe {self.data.get_unchecked(index)})
	}
	fn get_mut(&mut self, index: usize) -> Option<&mut E> {
		if index >= self.length as usize {return None}
		Some(unsafe {self.data.get_unchecked_mut(index)})
	}

	fn push(&mut self, item: E) -> () {
		self.data[self.length as usize] = item;
		self.length += 1;
	}
	fn pop_back(&mut self) -> () {
		if self.length == 0 {panic!("pop_back called on empty array")}
		self.length -= 1;
	}
}




pub fn get_primary_path() -> PathBuf {PathBuf::from_str(raw_primary_path).unwrap()}
pub fn get_secondary_path() -> PathBuf {PathBuf::from_str(raw_secondary_path).unwrap()}

#[macro_export]
macro_rules! unbind {
		($name:ident) => {
			#[expect(unused_variables)]
			let $name = ();
		};
}


pub fn ascertain_directory_exists(path: &Path) -> std::io::Result<()> {
	if !std::fs::exists(path)? {
		std::fs::create_dir(path)?;
	}
	else if !path.is_dir() {
		panic!();
		// return Err(std::io::Error::new(std::io::ErrorKind::Other, 0))
	}
	return Ok(());
}




pub fn growable_vec_insert<T: Clone>(vector: &mut Vec<T>, position: usize, item: impl FnOnce() -> T, default: impl FnOnce() -> T) {
	
	if position >= vector.len() {
		vector.resize(position+1, default());
	}
	vector[position] = item();
}



// #[derive(Debug)]
// pub enum Err_invalid_image {


// pub enum Load_png_indexed_problem {
//   not_indexed,
//   io_error(std::io::Error),
//   other(String),
// }
pub use anyhow::{anyhow, Result};

// #[derive(Debug)]
// pub enum Tiles_error {
// 	file_unreadable,
// 	metatile_image_bad_width,
// 	metatile_image_bad_height,
// 	image_bad_format,
// 	jasc_bad_header,
// 	jasc_row_count_mismatch
// }

// pub type Tiles_result<T = ()> = Result<T, anyhow::Error<>>;


pub trait Array_type<T>: IntoIterator<IntoIter: Iterator<Item = T>> {
	fn len(&self) -> usize;
	fn array_index(&self, index: usize) -> &T;
	// fn slice(&self) -> &[T];
}


impl<T, const size: usize> Array_type<T> for [T; size] {
	fn len(&self) -> usize {size}

	fn array_index(&self, index: usize) -> &T {
		&self[index]
	}

	// fn slice(&self) -> &[T] {
	// 	todo!()
	// }
}
impl<T> Array_type<T> for Vec<T> {
	fn len(&self) -> usize {
		Vec::len(&self)
	}

	fn array_index(&self, index: usize) -> &T {
		Vec::index(&self, index)
	}
}