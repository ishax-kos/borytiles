
use std::{collections::{BTreeMap, BTreeSet}, fs::File, io::{BufReader, Read, Write}, mem::transmute, ops::Index, path::PathBuf};


use image::{GrayImage, ImageBuffer, Rgba, RgbaImage};
// use num_traits::ops::bytes;

use crate::{helpers::*};

use super::palette::{Color24, Color_context, Indexed_color_set};
use std::hash::{Hash, Hasher};



#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Color_separated_tile {
	pub colors: BTreeMap<Tile_mask, u8>
}

impl PartialOrd for Color_separated_tile {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		let a: Vec<&Tile_mask> = self.colors.keys().collect();
		let b: Vec<&Tile_mask> = other.colors.keys().collect();
		a.partial_cmp(&b)
	}
}

impl Ord for Color_separated_tile {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		let a: Vec<&Tile_mask> = self.colors.keys().collect();
		let b: Vec<&Tile_mask> = other.colors.keys().collect();
		a.cmp(&b)
	}
}

impl Hash for Color_separated_tile {
	fn hash<H: Hasher>(&self, state: &mut H) {
		let v: Vec<_> = self.colors.keys().collect();
		v.hash(state);
	}
}



impl Color_context {
	fn fetch_tile_from_image(&mut self, tileset_image: &RgbaImage, base_x: u32, base_y: u32) -> (Color_separated_tile, Indexed_color_set)
	{
		let mut mask_set = BTreeMap::<u8, Tile_mask>::new();
		let mut color_set = Indexed_color_set::new();
		
		for y in 0..8 {
			for x in 0..8 {
				let rgba = tileset_image.get_pixel(base_x+x as u32, base_y+y as u32).0;
				let color = if let Some(c) = Color24::from_rgba(rgba) {
					c
				} else {continue;};
				let color_index = self.insert_get_index(color);
				
				if None == mask_set.get_mut(&color_index) {
					mask_set.insert(color_index, Tile_mask{rows:[0; 8]});
					color_set.set_bit(color_index);
				}
				
				let mask = mask_set.get_mut(&color_index).unwrap();
				mask.rows[y] |= 1 << x;
			}
		}
		
		let reversed_key_val = mask_set.into_iter().map(|(color,mask)|(mask,color));
		let tile = Color_separated_tile{colors:BTreeMap::from_iter(reversed_key_val)}.get_ideal_flip();
		(tile, color_set)
	}


	pub fn sequence_tiles(&mut self, 
		image: &RgbaImage, 
		tile_result: &mut BTreeSet<Color_separated_tile>, 
		color_result: &mut BTreeSet<Indexed_color_set>
	) {
		// let color_context = self;
		let width = image.width();
		assert!(width % 8 == 0);
		let height = image.height();
		assert!(height % 8 == 0);
		let tile_x_count = width / 8;
		let tile_y_count = height / 8;
		let expect_count = tile_x_count * tile_y_count;
		
		for i in 0..expect_count {
			let base_x = (i % tile_x_count) * 8;
			let base_y = (i / tile_x_count) * 8;

			let mut mask_set = BTreeMap::<u8, Tile_mask>::new();
			let mut color_set = Indexed_color_set::new();
			
			for tile_pixel in 0..64 {
				let x = tile_pixel % 8;
				let y = tile_pixel / 8;
				
				let pixel = image.get_pixel(base_x+x as u32, base_y+y as u32).0.into();
				let color = if let Some(c) = Color24::from_rgba(pixel) {c} else {continue;};
				let color_index = self.insert_get_index(color);
				
				if None == mask_set.get_mut(&color_index) {
					mask_set.insert(color_index, Tile_mask::new());
					color_set.set_bit(color_index);
				}
				
				let mask = mask_set.get_mut(&color_index).unwrap();
				mask.rows[y] |= 1 << x;
				// }
			}
			
			let reversed_key_val = mask_set.into_iter().map(|(color,mask)|(mask,color));
			let tile = Color_separated_tile{colors:BTreeMap::from_iter(reversed_key_val)}.get_ideal_flip();
			
			tile_result.insert(tile);
			color_result.insert(color_set);
		}
	}
}



#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, )]
pub struct Tile_mask {
	pub rows: [u8; 8]
}


impl Tile_mask {
	pub fn new() -> Self {
		Tile_mask { rows: [0;8] }
	}


	pub fn get_flip(&self, h: bool, v:bool) -> Self {
		if !h && !v {return self.clone()}
		let mut result = Tile_mask::new();
		let (v_inc, v_start): (i8, i8) = if v {(-1, 7)} else {(1, 0)};

		for y in 0..8 {
			let ry = y * v_inc + v_start;

			result.rows[y as usize] = if 
				h    {self.rows[ry as usize].reverse_bits() } 
				else {self.rows[ry as usize]}
		}
		result
	}
}

impl Color_separated_tile {
	pub fn get_flip(&self, h: bool, v:bool) -> Self {
		if !h && !v {return self.clone()}
		Color_separated_tile{colors:
			BTreeMap::from_iter(
				self.colors.iter().map(|(mask, color)| (mask.get_flip(h, v), *color))
			)
		}
	}

	pub fn get_ideal_flip(&self) -> Self {
		[
			self.get_flip(false, false),
			self.get_flip(false, true),
			self.get_flip(true, false),
			self.get_flip(true, true)
		].into_iter().max().unwrap()
	}
}


pub fn save_tileset_image(path: &Path, tiles: &Vec<[[u8;8];8]>) -> () {
	let mut output_image = GrayImage::new(128,256);
	output_image.fill(0);

	for (tile_id, tile) in tiles.into_iter().enumerate() {
		let tile_x = 8 * (tile_id % 16);
		let tile_y = 8 * (tile_id / 16);

		for (t_y, row) in tile.into_iter().enumerate() {
			for (t_x, cell) in row.into_iter().enumerate() {
				let color = 16 * (15-cell);
				output_image.put_pixel((tile_x + t_x) as u32, (tile_y + t_y) as u32, [color].into());
			}
		}
	};
	output_image.save_with_format(path, image::ImageFormat::Png).unwrap();
}