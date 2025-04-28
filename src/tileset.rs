
use std::{collections::{BTreeMap, BTreeSet}, mem::transmute, u8};


use image::GrayImage;
// use num_traits::ops::bytes;

use crate::{helpers::*, palette::{self, Color_index, Color_sequence, Palette_index, Palette_info, Palette_list}};

use super::palette::{Color24, Indexed_color_set};
use std::hash::{Hash, Hasher};



#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Shape_indexable_tile {
  pub colors: BTreeMap<Tile_mask, Color_index>
}

impl PartialOrd for Shape_indexable_tile {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    let a: Vec<&Tile_mask> = self.colors.keys().collect();
    let b: Vec<&Tile_mask> = other.colors.keys().collect();
    a.partial_cmp(&b)
  }
}

impl Ord for Shape_indexable_tile {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    let a: Vec<&Tile_mask> = self.colors.keys().collect();
    let b: Vec<&Tile_mask> = other.colors.keys().collect();
    a.cmp(&b)
  }
}

impl Hash for Shape_indexable_tile {
  fn hash<H: Hasher>(&self, state: &mut H) {
    let v: Vec<_> = self.colors.keys().collect();
    v.hash(state);
  }
}
impl Shape_indexable_tile {
  pub fn get_palette(&self, palettes: &Vec<Indexed_color_set>) -> u8 {
    let mut colors = Indexed_color_set::new();
    for (_mask, color_index) in self.colors.iter() {
      colors.set_bit(*color_index);
    }
    for (p, palette) in palettes.iter().enumerate() {
      if palette.is_superset_of(&colors) {
        return p as u8
      }
    }
    panic!()
  }
}
// #[derive(Debug, PartialEq, Eq, Clone)]
// pub struct Shape_indexable_tile_paletted {
// 	pub colors: BTreeMap<Tile_mask, u8>
// }

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct Tile_instance_intermediate {
  pub shape: Shape_indexable_tile,
  // pub palette: u8,
  pub flip_h: bool,
  pub flip_v: bool
}


#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
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

impl Shape_indexable_tile {
  pub fn get_flip(&self, h: bool, v:bool) -> Self {
    if !h && !v {return self.clone()}
    Shape_indexable_tile{colors:
      BTreeMap::from_iter(
        self.colors.iter().map(|(mask, color)| (mask.get_flip(h, v), *color))
      )
    }
  }

  pub fn get_ideal_flip(&self) -> Tile_instance_intermediate {
    let (shape, flip_h, flip_v) = [
      (false, false),
      (false, true),
      (true, false),
      (true, true)
    ].into_iter()
      .map(|p|(self.get_flip(p.0, p.1), p.0, p.1))
      .min().unwrap();
    Tile_instance_intermediate { shape, flip_h, flip_v }
  }
}


// pub fn save_tileset_image(path: &Path, tiles: &Vec<Tile>) -> () {
//   let mut output_image = GrayImage::new(128,256);
//   output_image.fill(0);

//   for (tile_id, tile) in tiles.into_iter().enumerate() {
//     let tile_x = 8 * (tile_id % 16);
//     let tile_y = 8 * (tile_id / 16);

//     for (t_y, row) in tile.rows.into_iter().enumerate() {
//       for (t_x, cell) in row.into_iter().enumerate() {
//         let color = 16 * (15-cell);
//         output_image.put_pixel((tile_x + t_x) as u32, (tile_y + t_y) as u32, [color].into());
//       }
//     }
//   };
//   output_image.save_with_format(path, image::ImageFormat::Png).unwrap();
// }



pub type Shape = BTreeSet<Tile_mask>;


#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Tile {
	pub rows: [[u8; 8]; 8]
}



#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Tile_4bpp {
  pub rows: [u32; 8]
}

impl Tile {
	pub fn new() -> Self {
		Self{rows: [[0u8; 8]; 8]}
	}
	pub fn from_shape_palette(
		tile_shape: &Shape_indexable_tile, 
		palettes: &Vec<(Indexed_color_set, [Option<Color_index>; 16])>
	) -> (Self, Palette_index) {
		let mut rows = [[0u8; 8]; 8];

		let color_set: Indexed_color_set = tile_shape.colors.values().cloned().collect();
		let palette_index = palettes.iter().position(|p|p.0.is_superset_of(&color_set)).unwrap();
		let palette = &palettes[palette_index].1;
		let palette_mapping = palette.into_iter().zip(0..).collect::<std::collections::HashMap<_, _>>();

		for (mask, color_index) in &tile_shape.colors {
			let color_index = Some(*color_index);
			let value = palette_mapping[&color_index];
			assert!(value <= 0b1111);
			for y in 0..8 {
				let mut row = mask.rows[y];
				for x in 0..8 {
					if row & 1 == 1 {
						rows[y][x] = value;
					}
					row >>= 1;
				}
			}
		}
		(Tile { rows }, Palette_index(palette_index as u8))
	}
}


impl From<Tile> for Tile_4bpp {
	fn from(value: Tile) -> Self {
		let mut rows_4bpp = [0u32; 8];
		for y in 0..8 {
			for x in 0..8 {
				let value = value.rows[y][x];
				assert!(value < 16);
				rows_4bpp[y] = (value as u32) << (x * 4);
			}
		}
		Tile_4bpp{rows: rows_4bpp}
	}
}

impl Tile_4bpp {
	pub fn new() -> Self {
		Self{rows: [0;8]}
	}

	// pub fn bitor_set_pixel(&mut self, x: usize, y: usize, value: u32) {
	// 	assert!(value < 16);
	// 	self.rows[y] = value << x;
	// }


}


#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Full_color_tile {
  rows: [[Color24; 8]; 8]
}


#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Tile_instance {
  pub tile_id: u16,
  pub flip_h: bool,
  pub flip_v: bool,
  pub palette_id: u8,
}

impl Tile_instance {
	pub fn new(tile_id: u16, flip_h: bool, flip_v: bool, palette_id: u8) -> Self {
		Tile_instance{
			tile_id,
			flip_h,
			flip_v,
			palette_id
		}
	}
  pub fn from_gba(value: u16) -> Self {
    Self::from_gba_pair(unsafe {transmute(value)})
  }
	pub fn from_gba_pair([lo, hi]:[u8;2]) -> Self {
		Self {
				 tile_id: (((hi & 0b11) as u16) << 8) | (lo as u16),
					flip_h: (hi & 0b0100) != 0,
					flip_v: (hi & 0b1000) != 0,
			palette_id: (hi >> 4) & 0b1111,
    }
	}
	pub fn to_gba(&self) -> u16 {
		let lo = (self.tile_id & 255) as u8;
		let hi = 0u8
			| (((self.tile_id & 0b11_00000000) >> 8) as u8)
			| (self.flip_h as u8) << 2
			| (self.flip_v as u8) << 3
			| (self.palette_id & 0b1111) << 4
		;
		unsafe {transmute([lo, hi])}
	}
}



// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct Metatile_quad (pub [Tile_instance; 4]);

// impl Metatile_quad {
//   pub fn new() -> Self {
//     Self([Tile_instance::new(0,false,false,0);4])
//   }

//   pub fn to_gba(&self) -> [u16; 4] {
//     let mut out = [0u16; 4];
//     for i in 0..4 {
//       out[i] = self.0[i].to_gba();
//     }
//     out
//   }
// }
