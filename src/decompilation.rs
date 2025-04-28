use std::{fs::read_dir, mem::transmute, path::Path};
use image::GrayImage;
use regex::Regex;
use crate::{compilation::Compiled_tileset, helpers::*, palette::{self, Color24}, tileset::{self, Tile, Tile_instance}};
 

pub fn decompile_primary(path: &Path, output_layer_names: &[&str]) {
	let output_path = path.join("decompiled_tileset");
	ascertain_directory_exists(&output_path).unwrap();
	let tileset = open_compiled_tileset(path, output_layer_names.len());
	let images = decompile_tileset(tileset);
	for i in 0..output_layer_names.len() {
		images[i].save_with_format(
			output_path.join(&output_layer_names[i]), 
			image::ImageFormat::Png
		).unwrap();
	}
}


pub fn open_compiled_tileset(path: &Path, layer_count: usize) -> Compiled_tileset {
  let tiles = load_tiles_png(path);
  let metatiles = load_metatileset(path).unwrap();
  let palettes = load_jasc_palette_folder(path);
	Compiled_tileset{ layer_count, metatiles, tiles, palettes }
}


pub fn decompile_tileset(compiled_tileset: Compiled_tileset) -> Vec<image::RgbaImage> {

	// let compiled_tileset = open_compiled_tileset(path, layer_count as _);
	let layer_count = compiled_tileset.layer_count;

  assert_eq!(compiled_tileset.metatiles.len() % layer_count, 0);
  let metatiles_tall = (compiled_tileset.metatiles.len() as u32).div_ceil(8 * layer_count as u32);
  let mut layer_images = Vec::with_capacity(layer_count);


  let image_size_x = 16 * 8;
  let image_size_y = 16 * metatiles_tall;

  for _ in 0..layer_count {
    layer_images.push(image::RgbaImage::new(image_size_x, image_size_y));
  }
  for (mt_i, metatile) in compiled_tileset.metatiles.chunks(layer_count * 4).enumerate() {
    let mt_x = 16*(mt_i % 8);
    let mt_y = 16*(mt_i / 8);

    for (layer_index, quad) in metatile.chunks(layer_count).enumerate() {
			
      for mtt_i in 0..4 {
				let Tile_instance { tile_id, flip_v, flip_h, palette_id } = quad[mtt_i];


        let mtt_x = 8*(mtt_i % 2);
        let mtt_y = 8*(mtt_i / 2);
 
 
        let tile = compiled_tileset.tiles[tile_id as usize];
        let palette = &compiled_tileset.palettes[palette_id as usize];
        // for (tp_y, row) in tile.into_iter().enumerate() {
        //   for (tp_x, cell) in row.into_iter().enumerate() {
        for (tp_y, row) in flip(tile.rows.into_iter(), flip_v).enumerate() {
          for (tp_x, cell) in flip(row.into_iter(), flip_h).enumerate() {
						let mut color: image::Rgba<u8>;
						color = palette::convert_optional_color(palette[cell as usize]);

            if cell == 0 {color.0[3] = 0;}
            let pixel_x = (mt_x+mtt_x+tp_x) as u32;
            let pixel_y = (mt_y+mtt_y+tp_y) as u32;
            
            layer_images[layer_index].put_pixel(
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



pub fn load_metatileset(path: &Path) -> std::io::Result<Vec<Tile_instance>> {
  let path = path.join("metatiles.bin");
  println!("{}", path.display());
  let raw_bytes = std::fs::read(path)?;
  assert!(raw_bytes.len() % 8 == 0);
  let mut result = Vec::new();
	for chunk2 in raw_bytes.chunks(2) {
		let pair: [u8;2] = chunk2.try_into().unwrap();
		let value = unsafe{transmute(pair)};
		let instance = Tile_instance::from_gba(value);
		result.push(instance);
  }
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


pub fn load_tiles_png(path: &Path) -> Vec<Tile> {
  let path = path.join("tiles.png");
  if let Some(image) = load_png_indexed(&path).unwrap() {
		let expect_count = image.width() * image.height() / 64;
		let tile_x_count = image.width() / 8;
		let mut result_vec = Vec::new();
		for i in 0..expect_count {
			let base_x = (i % tile_x_count) * 8;
			let base_y = (i / tile_x_count) * 8;
			let mut tile = Tile::new();
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
				tile.rows[y as usize][x as usize] = index % palette::palette_size_limit;
			}
			result_vec.push(tile);
		}
		return result_vec;
	}
  else {
		return load_tileset_not_indexed(&path);
	}
}

fn load_tileset_not_indexed(path: &Path) -> Vec<Tile> {
	let _ = path;
  todo!("Determine indices from luminosity");
}


pub fn load_jasc_palette_folder(path: &Path) -> Vec<[Option<Color24>; 16]> {
  let path = path.join("palettes");
  // path.is_dir();
  let mut palettes: Vec<[Option<Color24>; 16]> = vec![];

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
					||palette::read_jasc_palette(&path.join(file_name)).unwrap().try_into().unwrap(),
					||[None; 16]
				);
			}
		}
  }
  palettes
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

