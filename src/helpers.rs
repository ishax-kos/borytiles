pub use std::{
	collections::{ 
		HashSet, BTreeMap
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
