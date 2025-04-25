
use std::{collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet}, hash::Hash};



trait Bucket {
	type Item;
	fn bucket_push(&mut self, item: Self::Item);
	fn len(&self) -> usize;
	fn new_bucket() -> Self;
}

impl<T> Bucket for Vec<T> {
	type Item = T;
	fn bucket_push(&mut self, value: Self::Item) {
		self.push(value);
	}
	fn len(&self) -> usize {Vec::len(self)}
	
	fn new_bucket() -> Self {
		Self::new()
	}
	
}
impl<T: Eq+Hash> Bucket for HashSet<T> {
	type Item = T;
	fn bucket_push(&mut self, value: Self::Item) {
		HashSet::insert(self, value);
	}
	fn len(&self) -> usize {HashSet::len(self)}
	fn new_bucket() -> Self {
		Self::new()
	}
}
impl<T: Ord> Bucket for BTreeSet<T> {
	type Item = T;
	fn bucket_push(&mut self, value: Self::Item) {
		BTreeSet::insert(self, value);
	}
	fn len(&self) -> usize {BTreeSet::len(self)}
	fn new_bucket() -> Self {
		Self::new()
	}
}
impl<T: Ord> Bucket for BinaryHeap<T> {
	type Item = T;
	fn bucket_push(&mut self, value: Self::Item) {
		BinaryHeap::push(self, value);
	}
	fn len(&self) -> usize {BinaryHeap::len(self)}
	fn new_bucket() -> Self {
		Self::new()
	}
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
			let mut bucket = B::new_bucket();
			bucket.bucket_push(value);
			self.insert(key, bucket);
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
			let mut bucket = B::new_bucket();
			bucket.bucket_push(value);
			self.insert(key, bucket);
		}
	}
}
impl<V, B: Bucket<Item = V>> Bucket_map for Vec<B> {
	type Key = usize;

	type Value = V;

	fn bucket_insert(&mut self, index: Self::Key, value: Self::Value) {
		while index >= self.len() {
			self.push(B::new_bucket())
		}
		self[index].bucket_push(value);
	}
}



// impl<K, V, T> Bucket_map for BTreeMap<K, BTreeMap<V, BTreeSet<T>>> 
// where 
// K: Ord+Eq,
// V: Ord+Eq,
// Self: Bucket_map<Key = K, Value = V>,
// BTreeMap<V, BTreeSet<T>>: Bucket_map<Key = V, Value = BTreeSet<T>>,
// BTreeSet<T>: Bucket<Item = T>
// // BTreeMap<V, T>: 
// {
// 		type Key = K;
		 
// 		type Value = (V, T);


// 		fn bucket_insert(&mut self, key: Self::Key, value: Self::Value) {
// 				todo!()
// 		}
// }


// impl<A, B> Bucket for BTreeMap::<A, BTreeSet<B>> 
// where Self: Bucket_map<Key = A, Value = BTreeSet<B>> {
// 		type Item = (A, BTreeSet<B>);

// 		fn bucket_push(&mut self, (key, value): Self::Item) {
// 			self.bucket_insert(key, value);
// 		}
	
// 		fn len(&self) -> usize {
// 			self.len()
// 		}
	
// 		fn new_bucket() -> Self {
// 			BTreeMap::new()
// 		}
// }




// macro_rules! int_size {
// 	($size:expr) => {
// 		i$size
// 	};
// }


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





#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Bit_set<const size: usize> {
	pub bits: [u8; size]
}

impl<const size: usize> Bit_set<size> {
	pub fn new() -> Self {
		Bit_set{bits:[0;size]}
	}
	pub fn union(&self, other: &Self) -> Self {
		let mut uni = Self::new();
		for i in 0..size {
			uni.bits[i] = self.bits[i] | other.bits[i];
		}
		uni
	}
	pub fn intersection(&self, other: &Self) -> Self {
		let mut uni = Self::new();
		for i in 0..4 {
			uni.bits[i] = self.bits[i] & other.bits[i];
		}
		uni
	}

	pub fn len(&self) -> i16 {
		let mut count: i16 = 0;
		for group64 in self.bits {
			let mut bits = group64;
			for _ in 0..64 {
				count += (bits & 1) as i16;
				bits >>= 1;
			}
		}
		count
	}
	pub fn is_superset_of(&self, other: &Self) -> bool {
		self.union(other) == *self
	}
	pub fn is_subset_of(&self, other: &Self) -> bool {
		self.union(other) == *other
	}
	pub fn has_encompassing_relation(&self, other: &Self) -> Option<Self> {
		let un = self.union(other);
		if un ==  *self {return Some( *self)}
		if un == *other {return Some(*other)}
		return None
	}

	pub fn set_bit(&mut self, bit: u8) -> bool {
		let sub_bit = bit & 0b0011_1111;
		let section = bit >> 6;
		let set_mask = 1 << sub_bit;
		let prev = self.bits[section as usize] & set_mask;
		self.bits[section as usize] |= set_mask;
		return prev != 0;
	}
	pub fn unset_bit(&mut self, bit: u8) -> bool {
		let sub_bit = bit & 0b0011_1111;
		let section = bit >> 6;
		let get_mask = 1 << sub_bit;
		let unset_mask = !get_mask;
		let prev = self.bits[section as usize] & get_mask;
		self.bits[section as usize] &= unset_mask;
		return prev != 0;
	}

	pub fn get_bit(&self, bit: u8) -> bool {
		let sub_bit = bit & 0b0011_1111;
		let section = bit >> 6;
		let get_mask = 1 << sub_bit;
		
		return (self.bits[section as usize] & get_mask) != 0;
	}
}

impl FromIterator<u8> for Bit_set<32> {
	fn from_iter<T: IntoIterator<Item = u8>>(iter: T) -> Self {
		let mut set = Self::new();
		for index in iter {
			set.set_bit(index);
		}
		set
	}
}