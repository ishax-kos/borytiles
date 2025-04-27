

use crate::{bucket, decompilation::read_jasc_palette, helpers::*, unbind};
use std::{collections::{BTreeMap, BTreeSet}, f32::consts::TAU, fmt::Debug, fs::{exists, write}, i32, ops::{Index, IndexMut}, path::{Display, Iter, Path}, vec};
use clap::builder::styling::Color;
use image::{ImageFormat, ImageReader, Rgb, Rgba, RgbaImage};
use oklab::Oklab;

use super::tileset::Shape_indexable_tile;

pub static palette_size_limit: u8 = 16;

#[derive(Debug)]
pub struct Color_context {
	color_to_index: BTreeMap<Color24, Color_index>,
	index_to_color: Vec<Color24>,
}

impl Color_context {
	pub fn new() -> Self {
		Color_context{
			color_to_index: BTreeMap::new(), 
			index_to_color: vec![],
		}
	}
	pub fn push_iter<T: IntoIterator<Item = Color24>>(&mut self, iter: T) {
		for color in iter.into_iter() {
			self.insert_get_index(color);
		}
	}
	// pub fn insert_palette_get_index_set(&mut self, palette: [Color32; 16]) -> Indexed_color_set {
	// 	let mut set = Indexed_color_set::new();
	// 	for color in palette {
	// 		let index = self.insert_get_index(color);
	// 		set.set_bit(index);
	// 	}
	// 	set
	// }
	pub fn insert_get_index(&mut self, color: Color24) -> Color_index {
		if let Some(index) = self.color_to_index.get(&color) {
			return *index
		}
		let index = Color_index(self.index_to_color.len() as u8);
		self.color_to_index.insert(color, index);
		self.index_to_color.push(color);
		index
	}

	pub fn get_index(&self, color: Color24) -> Color_index {
		self.color_to_index[&color]
	}
	pub fn get_color(&self, Color_index(index): Color_index ) -> Color24 {
		self.index_to_color[index as usize]
	}
	pub fn get_color_rgba(&self, index: Option<Color_index> ) -> Rgba<u8> {
		if let Some(Color_index(index)) = index {
			self.index_to_color[index as usize].into()
		}
		else {
			Rgba([0;4])
		}
	}

	pub fn get_indexed_color_set<I>(&self, colors: I) -> Indexed_color_set
	where I : Iterator<Item = Color24> {
		let indices = colors.map(|color| self.get_index(color));
		Indexed_color_set::from_iter(indices)
	}
	pub fn insert_get_indexed_color_set<I>(&mut self, colors: I) -> Indexed_color_set
	where I : Iterator<Item = Color24> {
		let indices = colors.map(|color| self.insert_get_index(color));
		Indexed_color_set::from_iter(indices)
	}

	pub fn save_palette_image<'a, Iter>(&self, path: &Path, color_sets: Iter) -> ()
	where Iter: Iterator<Item = &'a Indexed_color_set> {
		let mut colors: Vec<Vec<Option<Color24>>> = Vec::new();

		for color_set in color_sets {
			colors.push(self.get_true_color_palette(color_set).into_iter().map(Some).collect());
		};

		write_palette_image(path, &colors);
	}

	pub fn get_true_color_palette(&self, color_set: &Indexed_color_set) -> Vec<Color24> {
		
		// println!("as bit set: {}", color_set.len());
		let mut color32_list = Vec::new();
		let mut i = 0;
		for group in color_set.bits {
			let mut mask = 1;
			for _ in 0..64 {
				if (mask & group) != 0 {
					color32_list.push(self.get_color(Color_index(i)));
				}
				mask <<= 1;
				if i == 255 {break}
				i += 1;
			}
		};
		// println!("converted: {}", color32_list.len());
		color32_list.sort_by(cmp_oklab_h);
		// // let result = organize_palette(color32_list);
		// println!("organized: {}", color32_list.len());
		color32_list
	}
}


fn wrapped_distance(distance: f32, a: f32, b: f32) -> f32 {
	distance/2.0 - (distance/2.0 - (b - a).abs()).abs()
}


#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Color24 {
	pub rgb: [u8; 3]
}

impl From<[u8; 3]> for Color24 {
	fn from(value: [u8; 3]) -> Self {
		Color24{rgb:value}
	}
}
impl Color24 {
	pub fn from_rgba(value: [u8; 4]) -> Option<Self> {
		if value[3] < 128 {
			return None;
		}

		Some(Color24{rgb:[value[0], value[1], value[2]]})
	}
}

pub const porytiles_magenta:Color24 = Color24{rgb:[248, 0, 248]};

pub fn convert_optional_color(color: Option<Color24>) -> Rgba<u8> {
	if let Some(color) = color {
		color.into()
	}
	else {
		Rgba::<u8>([0,0,0,0])
	}
}


#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Gba_color {
	pub bgr: u16
}

pub const gba_color_none: Gba_color = Gba_color{bgr:u16::MAX};
pub const gba_color_transparent: Gba_color = Gba_color{bgr:0b1000_0000_0000_0000};

impl Into<Option<Option<Color24>>> for Gba_color {
	fn into(self) -> Option<Option<Color24>> {
		let rgb = match self {
			gba_color_none => {return None},
			gba_color_transparent => {return Some(None)},
			Self{bgr} => {
				let   red = ((bgr >>  0) as u8 & 31) * 8;
				let green = ((bgr >>  5) as u8 & 31) * 8;
				let  blue = ((bgr >> 10) as u8 & 31) * 8;
				[red, green, blue]
			}
		};
		Some(Some(Color24{rgb: rgb}))
	}
}
impl Color24 {
	fn get_jasc_line(&self) -> String {
		let   red = self.rgb[0];
		let green = self.rgb[1];
		let  blue = self.rgb[2];
		format!("{red} {green} {blue}")
	}
}

impl From<Color24> for Gba_color {
	fn from(color: Color24) -> Self {

		// if color.rgb[3] < 128 {
		// 	return gba_color_transparent;
		// }

		let r = ((((color.rgb[0] + 4) / 8) as u16) & 31) << 0;
		let g = ((((color.rgb[1] + 4) / 8) as u16) & 31) << 5;
		let b = ((((color.rgb[2] + 4) / 8) as u16) & 31) << 10;
		
		Gba_color{bgr: r | g | b}
	}
}
impl From<Option<Color24>> for Gba_color {
	fn from(color: Option<Color24>) -> Self {

		if let Some(color) = color {
			return color.into();
		}

		return gba_color_transparent;
	}
}

pub fn read_palette_image(path: &Path) -> Vec<Vec<Option<Color24>>> {
	println!("{}", path.display());
	let image_data = ImageReader::open(path).unwrap()
		.with_guessed_format().unwrap()
		.decode().unwrap()
		.into_rgba8();
	
	let mut palette_collection = vec![vec![]; image_data.height() as usize];

	for y in 0..image_data.height() {
		let mut palette = Vec::new();
		for x in 0..image_data.width() {
			let color_from = Color24::from_rgba(image_data.get_pixel(x, y).0);
			palette.push(color_from);
		}
		
		if palette.len() == palette_size_limit as usize {
			let first_color = palette[0].clone();
			for color in palette.iter_mut() {
				if *color == first_color {
					*color = None;
				}
			}
		}

		if palette.iter().any(|c|c.is_some() && *c != palette[0]) {
			palette_collection[y as usize] = palette;
		}
	};
	
	println!("palettes {:?}", palette_collection.len());

	return palette_collection;
}



#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Color_index (u8);

// impl Color_index {
// 	pub fn transparent() -> Self {
// 		Color_index(0)
// 	}
// }

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Position_in_palette_concrete (u8);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum Position_in_palette {
	Undetermined,
	Absolute(u8),
	Indirect(Palette_index, Color_index)
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct Palette_info (pub BTreeMap<Color_index, Position_in_palette>, pub Option<Palette_index>);



#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Palette_index (pub u8);
pub type Color_sequence = Vec<Color_index>;


pub struct Palette_list(pub Vec<(Indexed_color_set, Palette_info)>);

impl Index<Palette_index> for Palette_list {
	type Output = (Indexed_color_set, Palette_info);

	fn index(&self, index: Palette_index) -> &Self::Output {
		&self.0[index.0 as usize]
	}
}
impl IndexMut<Palette_index> for Palette_list {
	fn index_mut(&mut self, index: Palette_index) -> &mut Self::Output {
		&mut self.0[index.0 as usize]
	}
}

impl Palette_list {
	pub fn insert_ics(&mut self, other: Indexed_color_set) {
		let index = other;
		let info = Palette_info(
			other.into_iter().map(|a|(a, Position_in_palette::Undetermined)).collect(),
			None
		);

		let a = index.clone();
		let b = info.0.keys().cloned().collect();
		assert_eq!(a, b);

		self.0.push((index, info));

	}
}



#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Indexed_color_set {
	pub bits: [u64;4]
}

impl Indexed_color_set {
	pub fn new() -> Self {
		Indexed_color_set{bits:[0;4]}
	}
	pub fn union(&self, other: &Self) -> Self {
		let mut uni = Self::new();
		for i in 0..4 {
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
		if un ==  *self {return Some(un)}
		if un == *other {return Some(un)}
		return None
	}

	pub fn set_bit(&mut self, bit: Color_index) -> bool {
		let Color_index(bit) = bit;
		let sub_bit = bit & 0b0011_1111;
		let section = bit >> 6;
		let set_mask = 1 << sub_bit;
		let prev = self.bits[section as usize] & set_mask;
		self.bits[section as usize] |= set_mask;
		return prev != 0;
	}
	pub fn unset_bit(&mut self, bit: Color_index) -> bool {
		let Color_index(bit) = bit;
		let sub_bit = bit & 0b0011_1111;
		let section = bit >> 6;
		let get_mask = 1 << sub_bit;
		let unset_mask = !get_mask;
		let prev = self.bits[section as usize] & get_mask;
		self.bits[section as usize] &= unset_mask;
		return prev != 0;
	}

	pub fn get_bit(&self, bit: Color_index) -> bool {
		let Color_index(bit) = bit;
		let sub_bit = bit & 0b0011_1111;
		let section = bit >> 6;
		let get_mask = 1 << sub_bit;
		
		return (self.bits[section as usize] & get_mask) != 0;
	}

	// pub fn to_vector(&self) -> Vec<Color_index> {
	// 	let mut accum = Vec::new();
	// 	for g in 0..4 {
	// 		let mut group = self.bits[g as usize].clone(); 
	// 		for b in 0..64 {
	// 			if (group & 1) == 1 {
	// 				accum.push(Color_index((g << 6) & b))
	// 			}
	// 			group <<= 1;
	// 		}
	// 	}
	// 	accum
	// }
}

impl FromIterator<Color_index> for Indexed_color_set {
	fn from_iter<T: IntoIterator<Item = Color_index>>(iter: T) -> Self {
		let mut set = Indexed_color_set::new();
		for index in iter {
			set.set_bit(index);
		}
		set
	}
}
impl FromIterator<Option<Color_index>> for Indexed_color_set {
	fn from_iter<T: IntoIterator<Item = Option<Color_index>>>(iter: T) -> Self {
		let mut set = Indexed_color_set::new();
		for index in iter {
			if let Some(index) = index {
				set.set_bit(index);
			}
		}
		set
	}
}

impl IntoIterator for Indexed_color_set {
	type IntoIter = Indexed_color_set_iter;
	
	type Item = Color_index;
	
	fn into_iter(self) -> Self::IntoIter {
		Indexed_color_set_iter{bits: self.bits, index: 0}
	}
}

pub struct Indexed_color_set_iter {
	bits: [u64; 4],
	index: u16,
}
impl Iterator for Indexed_color_set_iter {
	type Item = Color_index;

	fn next(&mut self) -> Option<Self::Item> {
		if self.index >= 256 {return None}
		let g = self.index >> 6;
		let b = self.index & 0b0011_1111;
		for g in g..4 {
			let group = &mut self.bits[g as usize]; 
			for _ in b..64 {
				let res = Color_index(self.index as u8);
				self.index += 1;
				let comp = (*group & 1) == 1;
				*group >>= 1;
				if comp {
					return Some(res);
				}
			}
		}
		return None;
	}
}

// impl std::fmt::Display for Indexed_color_set {
// 	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
// 		write!(f, "{:08X},{:08X},{:08X},{:08X}", self.bits[0], self.bits[1], self.bits[2], self.bits[3])
// 	}
// }
impl std::fmt::Debug for Indexed_color_set {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:064b},{:064b},{:064b},{:064b}", self.bits[0], self.bits[1], self.bits[2], self.bits[3])
	}
}



// type Normalized_tile = ();

// fn bucketize(tiles: Vec<Normalized_tile>) -> (
// 	BTreeMap::<Indexed_color_set, BTreeSet<Color_separated_tile>>,
// 	BTreeMap::<Color_separated_tile, BTreeSet<Indexed_color_set>>
// ) {
// 	let mut color_buckets = BTreeMap::<Indexed_color_set, BTreeSet<Color_separated_tile>>::new();
// 	let mut mask_buckets = BTreeMap::<Color_separated_tile, BTreeSet<Indexed_color_set>>::new();

// 	for tile in tiles {
// 		color_buckets.bucket_insert(tile.palette.clone(), tile.pixels.clone());
// 		mask_buckets.bucket_insert(tile.pixels, tile.palette);
// 	}
// 	(
// 		color_buckets,
// 		mask_buckets
// 	)
// }

macro_rules! _cartesian_triangle {
	($iterable:expr; ($a:ident, $b:ident) {$($stuff:tt)*}) => {
		for (ai, $a) in $iterable.iter().enumerate() {
			for (bi, $b) in ($iterable).iter().enumerate() {
				if ai < bi {
					$($stuff )*
				}
			}
		}
	};
}


pub fn filter_subsets(colors: &BTreeSet<Indexed_color_set>) -> BTreeSet<Indexed_color_set> {
	let challenger_pool = colors;
	let mut victor_pool = BTreeSet::<Indexed_color_set>::new();
	'l: for challenger in challenger_pool.iter() {
		let mut victor_out_pool = BTreeSet::<Indexed_color_set>::new();
		for victor in victor_pool.iter() {
			if challenger.is_superset_of(victor) {
				victor_out_pool.insert(*victor);
			}
			else if challenger.is_subset_of(victor) {
				continue 'l;
			}
		}
		for i in victor_out_pool.into_iter() {
			victor_pool.remove(&i);
		}
		victor_pool.insert(*challenger);
	}

	victor_pool
}

pub fn strip_subsets_naive(colors: &BTreeSet<Indexed_color_set>) -> BTreeSet<Indexed_color_set> {	
	// todo: replace Naive implementation
	let no_supersub_sets = colors.iter().enumerate()
		.filter(|(i_a, a)| 
			colors.iter().enumerate().all(|(i_b, b)| 
				!a.is_subset_of(b) || (*i_a == i_b)));

	no_supersub_sets.map(|(_, v)|*v).collect()
}


fn find_top_overlaps(all_color_sets: &BTreeSet<Indexed_color_set>) -> Running_best_by_score<Merged_color_sets> {
	let mut all_b = all_color_sets.clone();

	let mut best = Running_best_by_score::new();
	for color_set_a in all_color_sets.iter() {
		all_b.remove(&color_set_a);
		for color_set_b in all_b.iter() {
			if let Some((score, pair)) = Merged_color_sets::new([*color_set_a, *color_set_b]) {
				best.push(pair,score);
			}
		}
	}
	best
}


pub fn condense_palettes_by_overlap(color_set_pool: BTreeSet<Indexed_color_set>) -> BTreeSet<Indexed_color_set> {
	let mut color_set_pool = color_set_pool.clone();
	loop {
		color_set_pool = filter_subsets(&color_set_pool);
		let overlaps = find_top_overlaps(&color_set_pool);
		if overlaps.is_empty() {break}
		for Merged_color_sets{set, components} in overlaps {
			// If each component of the overlap is still in the pool, remove them and add their union to the pool
			if components.iter().all(|comp| color_set_pool.contains(comp)) {
				// for comp in components {
				// 	color_set_pool.remove(&comp);
				// 	if color_set_pool.is_empty() {break}
				// }
				color_set_pool.insert(set);
			}
		}
	}
	color_set_pool
}

pub fn condense_palettes_by_overlap_dfs(color_set_pool: BTreeSet<Indexed_color_set>) -> BTreeSet<Indexed_color_set> {
	// branch_stackbranch_stack
	let mut best = Running_best_by_score_single::new(BTreeSet::new(), i32::MIN+1);
	let mut stack: Vec<BTreeSet<_>> = vec![color_set_pool];

	while let Some(color_set_pool) = stack.pop() {
		let color_set_pool = filter_subsets(&color_set_pool);
		let overlaps = find_top_overlaps(&color_set_pool);
		if overlaps.is_empty() {
			let score = color_set_pool.len() as i32;
			if best.push(color_set_pool, -score) {
				println!();
			}
			print!("\rstack_len: {:3}, best: {}   ", stack.len(), -best.score);
			continue;
		}
		for overlap in overlaps {
			let mut color_set_pool = color_set_pool.clone();
			// color_set_pool.remove(&overlap.components[0]);
			// color_set_pool.remove(&overlap.components[1]);
			color_set_pool.insert(overlap.set);
			stack.push(color_set_pool);
		}
	}
	println!();
	best.best.to_owned()
}

pub fn condense_palettes_by_overlap_dfs_shortcut(color_set_pool: BTreeSet<Indexed_color_set>) -> BTreeSet<Indexed_color_set> {
	// branch_stackbranch_stack
	let mut best = Running_best_by_score::new();
	let mut stack: Vec<BTreeSet<_>> = vec![color_set_pool];

	while let Some(color_set_pool) = stack.pop() {
		let overlaps = find_top_overlaps(&color_set_pool);
		print!("\rstack_len: {:3}, best: {}            ", stack.len(), -best.score);
		if overlaps.is_empty() {
			// let color_sets = stack.pop().unwrap();
			let score = color_set_pool.len() as i32;
			// println!("pushing");
			best.push(color_set_pool, -score);
			continue;
		}

		let mut component_bucket = BTreeSet::new();
		let mut problem_pairs = BTreeSet::new();
		for pair in overlaps.best.iter() {
			// get all of the overlaps with components that overlap
			for comp in pair.components.iter() {
				if component_bucket.insert(*comp) {} 
				else {
					problem_pairs.insert(pair);
				}
			}
		}
		unbind!(component_bucket);
	
		if problem_pairs.is_empty() {
			let mut color_set_pool = color_set_pool.clone();
			remap_color_pool(&overlaps, &mut color_set_pool);
			stack.push(color_set_pool);
		}
		else {
			for troublemaker in problem_pairs.iter() {
				let mut color_set_pool = color_set_pool.clone();
				// for troublemaker in problem_pairs.iter() {
				// 	color_set_pool.remove(&troublemaker.set);
				// }
				color_set_pool.remove(&troublemaker.components[0]);
				color_set_pool.remove(&troublemaker.components[1]);
				color_set_pool.insert(troublemaker.set);
				remap_color_pool(&overlaps, &mut color_set_pool);
				stack.push(color_set_pool);
			}
		}
	}
	println!(".");
	best.best[0].to_owned()
}

fn remap_color_pool(overlaps: &Running_best_by_score<Merged_color_sets>, color_set_pool: &mut BTreeSet<Indexed_color_set>) {
	for pair in overlaps.best.iter() {
		for component in pair.components.iter() {
			color_set_pool.remove(component);
		}
		color_set_pool.insert(pair.set);
	}
}



#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct Merged_color_sets {
	set: Indexed_color_set,
	components: [Indexed_color_set;2]
}
impl Merged_color_sets {
	fn new(components: [Indexed_color_set;2]) -> Option<(i32, Merged_color_sets)> {
		let union = components[0].union(&components[1]);
		let union_len = union.len();
		if union_len > (palette_size_limit-1) as i16 {return None}
		if union_len <= 0 {return None}
		let inter = components[0].intersection(&components[1]);
		let score = inter.len();// - union_len; 
		Some((score as i32, Merged_color_sets{set:union, components}))
	}
}



impl Into<Rgba<u8>> for Color24 {
	fn into(self) -> Rgba<u8> {
		Rgba([
			self.rgb[0],
			self.rgb[1],
			self.rgb[2],
			255
		])		
	}
}

impl Into<oklab::Oklab> for Color24 {
	fn into(self) -> oklab::Oklab {
		oklab::srgb_to_oklab(oklab::Rgb::from(self.rgb))
	}
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
struct Averaged_colors {
	average: Color24,
	members: Vec<Color24>,
}
impl Averaged_colors {
	fn new(color: Color24) -> Self {
		Averaged_colors { average: color, members: vec![color] }
	}
	fn get_average(&self, other: &Self) -> Self {
		let mut new_members = Vec::with_capacity(self.members.len()+other.members.len());
		let mut average = [0u8; 3];
		for member in self.members.iter().chain(other.members.iter()) {
			average[0] += member.rgb[0];
			average[1] += member.rgb[1];
			average[2] += member.rgb[2];
			new_members.push(*member);
		}
		average[0] /= new_members.len() as u8;
		average[1] /= new_members.len() as u8;
		average[2] /= new_members.len() as u8;
		Averaged_colors { average: Color24 { rgb: average }, members: new_members }
	}
}



fn get_best_color_pair_by_distance(colors: &std::collections::HashSet<Averaged_colors>) -> Running_best_by_score<[Averaged_colors; 2]>  {
	let mut best = Running_best_by_score::new();
	for (a, color_a) in (&colors).into_iter().enumerate() {
		for (b, color_b) in (&colors).into_iter().enumerate() {
			if a < b {break}
			let distance = get_okhsl_distance(color_a.average, color_b.average);
			let score = -(distance * 255.0) as i32;
			best.push([color_a.clone(), color_b.clone()], score as i32);
		}
	}
	return best;
}


fn cmp_oklab_l(color_a: &Color24, color_b: &Color24) -> std::cmp::Ordering {
	let ok_a: oklab::Oklab = (*color_a).into();
	let ok_b: oklab::Oklab = (*color_b).into();
	let ok_a: Okhsl = ok_a.into();
	let ok_b: Okhsl = ok_b.into();
	(ok_a.l).total_cmp(&ok_b.l)
}
fn cmp_oklab_h(color_a: &Color24, color_b: &Color24) -> std::cmp::Ordering {
	let ok_a: oklab::Oklab = (*color_a).into();
	let ok_b: oklab::Oklab = (*color_b).into();
	let ok_a: Okhsl = ok_a.into();
	let ok_b: Okhsl = ok_b.into();
	(ok_a.h).total_cmp(&ok_b.h)
}
fn cmp_oklab_average_h(av: &Vec<Color24>, bv: &Vec<Color24>) -> std::cmp::Ordering {
	use oklab::{Rgb, srgb_to_oklab};
	let len = av.len() as u32;
	let a = av.into_iter().fold([0u32;3], |a, c|[a[0]+c.rgb[0] as u32,a[1]+c.rgb[1] as u32,a[1]+c.rgb[1] as u32]);
	let b = bv.into_iter().fold([0u32;3], |a, c|[a[0]+c.rgb[0] as u32,a[1]+c.rgb[1] as u32,a[1]+c.rgb[1] as u32]);
	let a:[u8;3] = a.into_iter().map(|w:u32|(w/len) as u8).collect::<Vec<u8>>().try_into().unwrap();
	let b:[u8;3] = b.into_iter().map(|w:u32|(w/len) as u8).collect::<Vec<u8>>().try_into().unwrap();

	let ok_a: oklab::Oklab = srgb_to_oklab(Rgb::from(a));
	let ok_b: oklab::Oklab = srgb_to_oklab(Rgb::from(b));
	let ok_a: Okhsl = ok_a.into();
	let ok_b: Okhsl = ok_b.into();
	ok_a.h.total_cmp(&ok_b.h)
}
fn get_okhsl_distance(color_a: Color24, color_b: Color24) -> f32 {
	let ok_a: oklab::Oklab = color_a.into();
	let ok_b: oklab::Oklab = color_b.into();
	let ok_a: Okhsl = ok_a.into();
	let ok_b: Okhsl = ok_b.into();
	// let sum = wrapped_distance(TAU, ok_a.h, ok_b.h)//.powi(2) 
		// + (ok_a.s - ok_b.s).powi(2) 
		// + (ok_a.l - ok_b.l).powi(2)
	// ;
	// sum//.sqrt()
	(ok_a.s - ok_b.s).abs()
}

enum Color_or_set {
	set(BTreeSet<Color24>),
	color(Color24)
}


struct Okhsl {
	h: f32,
	s: f32,
	l: f32,
}
impl From<Oklab> for Okhsl {
	
	fn from(value: Oklab) -> Self {
		Self { 
			h: f32::atan2(value.b,value.a), 
			s: (value.b.powi(2) + value.a.powi(2)).sqrt(), 
			l: value.l
		}
	}
}

 
pub fn write_palette_image(
	path: &Path,
	palettes: &Vec<impl Array_type<Option<Color24>>>, 
) -> () {
	let colors = palettes.into_iter();
	let mut output_image = RgbaImage::new(16,colors.len() as u32);
	output_image.fill(0); 

	for (y, color_set) in colors.enumerate() {
		for x in 0..color_set.len() {
			if let Some(color) = color_set.array_index(x) {
				let color: Color24 = color.clone();
				output_image.put_pixel(x as u32, y as u32, color.into());
			}
		}
	};
	output_image.save_with_format(path, ImageFormat::Png).unwrap();
} 

pub fn write_jasc_palettes(
	path: &Path, 
	palettes: &Vec<impl Array_type<Option<Color24>>>, 
	fallback: Option<Color24>
) -> () {
	let fallback = if let Some(color) = fallback {
			&color.get_jasc_line()
		}
		else {"-"}
	;
	for i in 0..palettes.len() {
		let palette = &palettes[i];
		let mut string = format!("JASC-PAL\n0100\n16\n");
		for c in 0..16usize {
			if c < palette.len() {
				if let Some(color) = palette.array_index(c) {
					let line = color.get_jasc_line();
					string.push_str(&line);
					string.push_str("\n");
					continue;
				}
			}
			string.push_str(fallback);
			string.push_str("\n");
		}
		write(path.join(format!("{i:02}.pal")), string).unwrap();
	}
}


pub fn load_palette_overrides(color_context: &mut Color_context) -> Palette_list {
	let pal_image_path = PathBuf::from("override_palette_path");
	let pal_jasc_path = PathBuf::from("override_palette_path");

	let mut colors_sparse = vec![];

	if exists(&pal_image_path).unwrap() {
		colors_sparse = read_palette_image(&pal_image_path);		

	} else if exists(&pal_jasc_path).unwrap() {

		// see if jasc palette overrides exist
		colors_sparse = crate::decompilation::load_jasc_palette_folder(&pal_jasc_path);

		// let c = colors_sparse.iter().map(|a|a.clone().unwrap_or(vec![])).collect();

		write_palette_image(&pal_jasc_path.join("colors.png"), &colors_sparse);
	}
	let mut result = Vec::new();

	for (i, palette) in colors_sparse.into_iter().enumerate() {
		if palette.len() > 0 {
			let mut color_set = Indexed_color_set::new();
			let mut color_map = BTreeMap::new();
			for (c, color) in palette.into_iter().enumerate() {
				if let Some(color) = color {
					let color_index = color_context.insert_get_index(color);
					color_set.set_bit(color_index);
					color_map.insert(color_index, Position_in_palette::Absolute(c as u8));
				}
			}
			result.push((color_set, Palette_info(color_map, Some(Palette_index(i as u8)))));
		}
	}
	Palette_list(result)
}


trait Color_row {
	fn get_color_row(&self) -> impl Iterator<Item = Option<Color24>>;
}

impl Color_row for Vec<Color24> {
	fn get_color_row(&self) -> impl Iterator<Item = Option<Color24>> {
		self.iter().map(|a|Some(a.clone()))
	}
}

impl Color_row for Vec<Option<Color24>> {
	fn get_color_row(&self) -> impl Iterator<Item = Option<Color24>> {
		self.iter().cloned()
	}
}

impl Color_row for Option<Vec<Color24>> {
	fn get_color_row(&self) -> impl Iterator<Item = Option<Color24>> {
		self.iter().cloned().flatten().map(Some)
	}
}

impl Color_row for Option<Vec<Option<Color24>>> {
	fn get_color_row(&self) -> impl Iterator<Item = Option<Color24>> {
		self.iter().cloned().flatten()
	}
}

