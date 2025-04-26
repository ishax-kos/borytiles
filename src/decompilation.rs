use std::{fs::{read_dir, read_to_string}, path::{Path, PathBuf}};
use image::GrayImage;
use regex::Regex;
use crate::{helpers::*, palette::{self, Color24}, tileset};
 


pub fn decompile_primary(path: &Path, number_of_layers: usize) -> Vec<image::RgbaImage> {
  let tiles = load_tileset(path);
  let metatiles = load_metatileset(path).unwrap();
  let palettes = load_jasc_palette_folder(path);
  assert_eq!(metatiles.len() % number_of_layers, 0);
  let metatiles_tall = (metatiles.len() as u32).div_ceil(8 * number_of_layers as u32);
  let mut layer_images = Vec::with_capacity(number_of_layers);

  
  palette::write_palette_image(&PathBuf::from("colors.png"), &palettes);
  tileset::save_tileset_image(&PathBuf::from("tiles.png"), &tiles);

  let image_size_x = 16 * 8;
  let image_size_y = 16 * metatiles_tall;

  for _ in 0..number_of_layers {
    layer_images.push(image::RgbaImage::new(image_size_x, image_size_y));
  }
  for (mt_i, metatile) in metatiles.chunks(number_of_layers).enumerate() {
    let mt_x = 16*(mt_i % 8);
    let mt_y = 16*(mt_i / 8);

    for (layer, metatile_layer) in metatile.into_iter().enumerate() {

      for (mtt_i, Metatile_tile { tile_id, flip_v, flip_h, palette_id }) in metatile_layer.0.into_iter().enumerate() {
        let mtt_x = 8*(mtt_i % 2);
        let mtt_y = 8*(mtt_i / 2);
 
 
        let tile = tiles[tile_id as usize];
        let palette = &palettes[palette_id as usize];
        // for (tp_y, row) in tile.into_iter().enumerate() {
        //   for (tp_x, cell) in row.into_iter().enumerate() {
        for (tp_y, row) in flip(tile.into_iter(), flip_v).enumerate() {
          for (tp_x, cell) in flip(row.into_iter(), flip_h).enumerate() {
						let mut color: image::Rgba<u8>;
						color = palette::convert_optional_color(palette[cell as usize]);

            if cell == 0 {color.0[3] = 0;}
            let pixel_x = (mt_x+mtt_x+tp_x) as u32;
            let pixel_y = (mt_y+mtt_y+tp_y) as u32;
            
            layer_images[layer].put_pixel(
              pixel_x,
              pixel_y, 
              color.into()  
            );
          }
        }
      }
    }
  }
  layer_images
}




#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Metatile_layer ([Metatile_tile; 4]);

impl Metatile_layer {
  pub fn new() -> Self {
    Self([Metatile_tile::new();4])
  }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Metatile_tile {
  pub(crate) tile_id: u16, 
  pub(crate) flip_v: bool, 
  pub(crate) flip_h: bool, 
  pub(crate) palette_id: u8
}


impl Metatile_tile {
  pub fn new() -> Self {
    Self{tile_id:0, flip_v:false, flip_h: false, palette_id:0}
  }
}


pub fn load_metatileset(path: &Path) -> std::io::Result<Vec<Metatile_layer>> {
  let path = path.join("metatiles.bin");
  println!("{}", path.display());
  let raw_bytes = std::fs::read(path)?;
  assert!(raw_bytes.len() % 8 == 0);
  let mut result = Vec::new();
  for chunk8 in raw_bytes.chunks(8) {
    let mut metatile = Metatile_layer::new();
    for (i, chunk2) in chunk8.chunks(2).enumerate() {
      let lo = chunk2[0];
      let hi = chunk2[1];
      metatile.0[i] = Metatile_tile {
           tile_id: (((hi & 0b11) as u16) << 8) | (lo as u16),
            flip_h: (hi & 0b0100) != 0,
            flip_v: (hi & 0b1000) != 0,
        palette_id: (hi >> 4) & 0b1111,
      };
    }
    result.push(metatile);
  };
  Ok(result)
}


pub fn load_png_indexed(path: &Path) -> Result<Option<GrayImage>> {
  let decoder = png::Decoder::new(std::fs::File::open(path)?);
  let mut reader = decoder.read_info()?;
	match reader.info().color_type {
		png::ColorType::Indexed | png::ColorType::Grayscale => {},
		_ => {
			// return Err(anyhow!("PNG is not indexed or in grayscale"));
			return Ok(None);
		}
	} 
  let mut buf = vec![0; reader.output_buffer_size()];
  let info = reader.next_frame(&mut buf)?;
  let bytes = &buf[..info.buffer_size()];
  let info = reader.info();
  let pixels : Vec<u8> = match info.bit_depth {
      png::BitDepth::One => todo!(),
      png::BitDepth::Two => bytes.into_iter().flat_map(|byte| [
        (byte >> 6) & 0b11,
        (byte >> 4) & 0b11, 
        (byte >> 2) & 0b11, 
        (byte >> 0) & 0b11, 
        ]).collect(),
      png::BitDepth::Four => bytes.into_iter().flat_map(|byte| [
        (byte >> 4) & 0b1111,
        (byte >> 0) & 0b1111, 
        ]).collect(),
      png::BitDepth::Eight => bytes.to_vec(),
      png::BitDepth::Sixteen => panic!(),
  };
  
  Ok(Some(GrayImage::from_raw(info.width,info.height,pixels).unwrap()))
}


pub fn load_tileset(path: &Path) -> Vec<[[u8; 8]; 8]> {
  let path = path.join("tiles.png");
  if let Some(image) = load_png_indexed(&path).unwrap() {
		let expect_count = image.width() * image.height() / 64;
		let tile_x_count = image.width() / 8;
		let mut result_vec = Vec::new();
		for i in 0..expect_count {
			let base_x = (i % tile_x_count) * 8;
			let base_y = (i / tile_x_count) * 8;
			let mut tile = [[0u8; 8]; 8];
			// Each entire tile must fit within a 16 value range.
			let mut range = Option::<u8>::None;
			for tile_pixel in 0..64u32 {
				let x = tile_pixel % 8;
				let y = tile_pixel / 8;
				let index = image.get_pixel(base_x+x, base_y+y).0[0];
				if let Some(range) = range {
					if index / palette::palette_size_limit != range
					{return load_tileset_not_indexed(&path);}
				}
				else {
					range = Some(index / palette::palette_size_limit);
				}
				tile[y as usize][x as usize] = index % palette::palette_size_limit;
			}
			result_vec.push(tile);
		}
		return result_vec;
	}
  else {
		return load_tileset_not_indexed(&path);
	}
}

fn load_tileset_not_indexed(path: &Path) -> Vec<[[u8; 8]; 8]> {
	let _ = path;
  todo!("Determine indices from luminosity");
}


pub fn load_jasc_palette_folder(path: &Path) -> Vec<Vec<Option<Color24>>> {
  let path = path.join("palettes");
  // path.is_dir();
  let mut palettes = vec![];

	let name_matcher = Regex::new(r"(\d\d)\.pal").unwrap();
	// let file_names = vec![];
	
  for entry in read_dir(&path).unwrap() {
		if let Ok(entry) = entry {
			let file_name = entry.file_name();
			let file_name = file_name.to_str().unwrap();
			if let Some(captures) = name_matcher.captures(file_name) {
				let match_string = captures.get(1).unwrap().as_str();
				// println!("the number text is {match_string}");
				let number = match_string.parse::<usize>().unwrap();
				if number >= 16 {
					eprintln!("Palette count exceeded 16");
					break;
				}
				growable_vec_insert(&mut palettes, number,
					||read_jasc_palette(&path.join(file_name)).unwrap(),
					||vec![]
				);
			}
		}
  }
  palettes
}

pub fn read_jasc_palette<'a>(path: &Path) -> Result<Vec<Option<Color24>>> {
  let text = read_to_string(path).unwrap();
  let mut lines = text.lines();
  if lines.next() == Some("JASC-PAL") && lines.next() == Some("0100") {
    let color_count = usize::from_str_radix(lines.next().unwrap(), 10).unwrap();
    let mut colors = Vec::with_capacity(color_count);
    for line in lines {
      let mut color = [0u8;3];
			let line = line.trim();
			if line.trim() == "-" {
				colors.push(None);
			}
			else {
				for (c, num) in line.split(" ").enumerate() {
					color[c] = u8::from_str_radix(num, 10).unwrap();
				}
				colors.push(Some(Color24::from(color)));
			}
    }
    if colors.len() != color_count {
			// return Err(Tiles_error::jasc_row_count_mismatch)
			let file = path.file_name().unwrap().to_str().unwrap();
			let color_rows = colors.len();
			return Err(anyhow!(
				"palette file {file} of size {color_count} does not match number of color rows {color_rows}"
			))
		}
    return Ok(colors);
  }
  else {
		// return Err(Tiles_error::jasc_bad_header)
    return Err(anyhow!("missing JASC header on {}", path.display()))
  }
}

struct FlipIter<I: DoubleEndedIterator> {
  inner: I,
  flipped: bool
}

impl<I: DoubleEndedIterator> Iterator for FlipIter<I> {
  type Item = I::Item;
  
  fn next(&mut self) -> Option<Self::Item> {
    if self.flipped {self.inner.next_back()} else {self.inner.next()}
  }
}

fn flip<In, I, T>(item: In, flip: bool) -> FlipIter<I>
where In: IntoIterator<IntoIter = I>, I: DoubleEndedIterator<Item = T> {
  FlipIter::<I>{inner:item.into_iter(), flipped:flip}
}	
// trait Flippable: DoubleEndedIterator+Sized {
// 	fn flip(self, flipped: bool) -> FlipIter<Self> {
// 		FlipIter{inner:self, flipped}
// 	}
// }

