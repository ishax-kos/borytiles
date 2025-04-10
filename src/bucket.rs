
use std::{collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet}, hash::Hash};



trait Bucket: IntoIterator+FromIterator<Self::Item> {
	fn bucket_push(&mut self, item: Self::Item);
	fn len(&self) -> usize;
}

impl<T> Bucket for Vec<T> {
	fn bucket_push(&mut self, value: Self::Item) {
		self.push(value);
	}
	fn len(&self) -> usize {Vec::len(self)}
}
impl<T: Eq+Hash> Bucket for HashSet<T> {
	fn bucket_push(&mut self, value: Self::Item) {
		HashSet::insert(self, value);
	}
	fn len(&self) -> usize {HashSet::len(self)}
}
impl<T: Ord> Bucket for BTreeSet<T> {
	fn bucket_push(&mut self, value: Self::Item) {
		BTreeSet::insert(self, value);
	}
	fn len(&self) -> usize {BTreeSet::len(self)}
}
impl<T: Ord> Bucket for BinaryHeap<T> {
	fn bucket_push(&mut self, value: Self::Item) {
		BinaryHeap::push(self, value);
	}
	fn len(&self) -> usize {BinaryHeap::len(self)}
}

pub(crate) trait Bucket_map {
	type Key;
	type Value; 
	fn bucket_insert(&mut self, key: Self::Key, value: Self::Value);
}

impl<K: Hash+Eq, V, B: Bucket<Item = V>> Bucket_map for HashMap<K, B> {
	type Key = K;

	type Value = V;

	fn bucket_insert(&mut self, key: Self::Key, value: Self::Value) {
		if let Some(bucket) = self.get_mut(&key) {
			Bucket::bucket_push(bucket, value);
		}
		else {
			self.insert(key, [value].into_iter().collect());
		}
	}
}
impl<K: Ord+Eq, V, B: Bucket<Item = V>> Bucket_map for BTreeMap<K, B> {
	type Key = K;

	type Value = V;

	fn bucket_insert(&mut self, key: Self::Key, value: Self::Value) {
		if let Some(bucket) = self.get_mut(&key) {
			Bucket::bucket_push(bucket, value);
		}
		else {
			self.insert(key, [value].into_iter().collect());
		}
	}
}


macro_rules! int_size {
	($size:expr) => {
		i$size
	};
}


fn iter_to_lut_255<I: IntoIterator<Item=T>, T: Copy+Ord+Eq>(iter: I) -> BTreeMap<T, u8> {
	let mut out = BTreeMap::new();
	let mut i = 0;
	for v in iter {
		out.insert(v, i);
		i = i + 1;
	}
	out
}


#[derive(Debug, Clone)]
pub struct Running_best_by_score<T> {
	pub best: Vec<T>,
	pub score: i32
}
impl<T> Running_best_by_score<T> {
	pub fn new() -> Self {
		// println!("new");
		Self{best:vec![], score: i32::MIN+1}
	}
	pub fn push(&mut self, item: T, score: i32) {
		// println!("it ran {}", self.best.len());
		match self.score.cmp(&score) {
			std::cmp::Ordering::Less => {
				self.best = vec![item];
				self.score = score
			},
			std::cmp::Ordering::Equal => self.best.push(item),
			std::cmp::Ordering::Greater => (),
		}
	}

	pub fn is_empty(&self) -> bool {
		self.best.is_empty()
	}
}

impl<T> IntoIterator for Running_best_by_score<T> {
	type Item = T;

	type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

	fn into_iter(self) -> Self::IntoIter {
		self.best.into_iter()
	}
}#[derive(Debug, Clone)]
pub struct Running_best_by_score_single<T> {
	pub best: T,
	pub score: i32
}
impl<T> Running_best_by_score_single<T> {
	pub fn new(value: T, score: i32) -> Self {
		// println!("new");
		Self{best:value, score}
	}
	pub fn push(&mut self, item: T, score: i32) -> bool {
		// println!("it ran {}", self.best.len());
		match self.score.cmp(&score) {
			std::cmp::Ordering::Less => {
				self.best = item;
				self.score = score;
				return true;
			},
			std::cmp::Ordering::Equal => (),
			std::cmp::Ordering::Greater => (),
		}
		return false;
	}
}
