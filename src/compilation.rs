
use image::*;
use crate::palette::{
	self, load_palette_overrides, porytiles_magenta, write_jasc_palettes, write_palette_image, Color24, Color_context, Color_index, Color_sequence, Indexed_color_set, Palette_index, Palette_info, Palette_list
};

use crate::helpers::*;
use crate::tileset::{Shape, Shape_indexable_tile, Tile_4bpp, Tile_instance, Tile_instance_intermediate, Tile_mask};
use core::panic;
use std::fs;

use std::collections::BTreeSet;



pub fn compile_primary(tileset_path: &Path, layer_names: &[&str]) -> Result<()> {
	let images: Vec<RgbaImage> = layer_names.into_iter()
		.map(|name| open_metatiles_image(&tileset_path.join(name)))
		.collect();
	let tileset = compile_tileset(images)?;

	tileset.write(tileset_path)?;
	Ok(())
}


pub fn compile_tileset(images: Vec<RgbaImage>) -> Result<Compiled_tileset> {

	let mut color_context = Color_context::new();
	let mut tiles = BTreeSet::new();
	let mut colors = BTreeSet::new();
	let mut metatile_maps = Vec::<Vec<Tile_instance_intermediate>>::new();
	for layer_image in images.iter() {
		metatile_maps.push(color_context.process_metatile_image(&layer_image, &mut tiles, &mut colors));
	}
	
	color_context.save_palette_image(&PathBuf::from("./neat_little_pal.png"), colors.iter());

	let overrides: Palette_list = load_palette_overrides(&mut color_context);

	let colors = crate::palette::filter_subsets(&colors);
	let colors = crate::palette::condense_palettes_by_overlap(colors);
	
	let palettes_by_number = assign_palettes(tiles, colors, overrides);

	let palettes = colorize_palettes(palettes_by_number.iter().map(|a|a.1), &color_context);

	let (metatiles, tiles) = construct_metatile_buffer(metatile_maps, &palettes_by_number);

	Ok(Compiled_tileset {
		metatiles,
		tiles,
		palettes
	})
}


pub fn colorize_palettes(
	palettes: impl Iterator<Item = [Option<Color_index>; 16]>,
	color_context: &Color_context
) -> Vec<[Option<Color24>; 16]>
{
	palettes.map(|row| {
		let mut out_row = [None;16];
		for (color, color_index) in out_row.iter_mut().zip(row.iter()) {
			if let Some(color_index) = color_index {
				*color = Some(color_context.get_color(*color_index))
			}
		}
		out_row
	}).collect()
}


pub struct Compiled_tileset {
	metatiles: Vec<Tile_instance>,
	tiles: Vec<Tile_4bpp>,
	palettes: Vec<[Option<Color24>; 16]>
}


impl Compiled_tileset {
	pub fn write(&self, path: &Path) -> Result<()> {
		save_tile_image(&path, &self.tiles);
		save_palettes(&path, &self.palettes);
		save_metatiles(&path, &self.metatiles);
		Ok(())
	}
}


fn save_palettes(
	path: &Path,
	palettes: &Vec<[Option<Color24>; 16]>,
) {
	
	let jasc_path = path.join("palettes");
	ascertain_directory_exists(&jasc_path).unwrap();
	write_jasc_palettes(&jasc_path, palettes, Some(porytiles_magenta));
	write_palette_image(&path.join("palette.png"), palettes);
}


fn save_metatiles(
	path: &Path,
	baked_metatiles: &Vec<Tile_instance>,
) {
	let hardware_mt: Vec<_> = baked_metatiles.iter().map(Tile_instance::for_hardware).collect();
	fs::write(path.join("metatiles.bin"), hardware_mt.as_bytes()).unwrap();
}


fn construct_metatile_buffer(
	metatile_maps: Vec<Vec<Tile_instance_intermediate>>, 
	// tiles: &BTreeSet<Shape_indexable_tile>, 
	palettes: &Vec<(Indexed_color_set, [Option<Color_index>; 16])>,
) -> (Vec<Tile_instance>, Vec<Tile_4bpp>)  {
	let metatiles = interleave_metatiles(metatile_maps);

	// let tile_map = tiles.iter().cloned().zip(0u16..).collect::<BTreeMap<_,_>>();
	let mut tile_map = std::collections::HashMap::<Tile_4bpp, u16>::new();
	let mut tile_list = Vec::<Tile_4bpp>::new();
	{
		let tile = Tile_4bpp{rows: [0;8]};
		tile_list.push(tile);
		tile_map.insert(tile, 0);
	}
 
	(metatiles.map(|tile_instance| {
		let tile_shape = &tile_instance.shape;
		let (tile, palette_index) = Tile_4bpp::from_shape_palette(tile_shape, palettes);
		let tile_id: u16;
		if let Some(&id) = tile_map.get(&tile) {
			tile_id = id;
		}
		else {
			tile_id = tile_list.len() as u16;
			tile_list.push(tile);
			tile_map.insert(tile, tile_id);
		}
		
		Tile_instance::new(tile_id, tile_instance.flip_h, tile_instance.flip_v, palette_index.0)
	}).collect(), tile_list)
}


fn save_tile_image(
	path: &Path,
	tiles: &Vec<Tile_4bpp>,
) {
	let tile_count = tiles.len();
	const tiles_wide: usize = 16;
	const pixels_wide: usize = 8 * tiles_wide;
	let tiles_high: usize = tile_count.div_ceil(16);
	let pixels_high: usize = tiles_high * 8;

	let mut data = vec![0u8; pixels_wide/2 * pixels_high];
	// assert_eq!(pixels_wide*pixels_high, tiles.len()*64);
	for (tile_index, tile) in tiles.into_iter().enumerate() {
		let tile_x = tile_index % tiles_wide * 8;
		let tile_y = tile_index / tiles_wide * 8;

		
		for y in 0..8 {
			let mut row: u32 = tile.rows[y];
			for x in 0..8 {
				let bits = (row & 0b1111) as u8;
				
				let x_byte = tile_x + x;
				let y_byte = tile_y + y;


				let pixel_index = x_byte + (y_byte * 128);
				let shift = ((x+1) % 2) * 4;
				data[pixel_index/2] |= bits << shift;
				row >>= 4;
				
			}
		}

	}

	

	let file = fs::File::create(path.join("tiles.png")).unwrap();
	let ref mut writer = std::io::BufWriter::new(file);
	let mut encoder = png::Encoder::new(writer, pixels_wide as u32, pixels_high as u32);
	encoder.set_depth(png::BitDepth::Four);
	encoder.set_color(png::ColorType::Grayscale);
	let mut writer = encoder.write_header().unwrap();
	writer.write_image_data(&data).unwrap();
}


fn validate_metatiles_image(image: &ImageBuffer<Rgba<u8>, Vec<u8>>) -> Result<()> {
	// let mut errors = vec![];
	if image.width() % 16 != 0 {
		return Err(anyhow!("Metatile layer image width is not a multiple of 16!"));
	}
	if image.height() % 16 != 0 {
		return Err(anyhow!("Metatile layer image height is not a multiple of 16!"));
	}

	return Ok(());
}


// fn show_user_unmappable_tiles(tiles: &Vec<SubImage<&RgbaImage>>, palettes_mapping: &Vec<Option<u8>>) {
// 	todo!("Output a debug image with the full color tiles that don't work for different reasons.")
// }


pub fn open_metatiles_image(path: &Path) -> RgbaImage {
	let image_data = 
		match ImageReader::open(&path) {
			Ok(result) => result,
			Err(e) => panic!("{}\n{}", path.display(), e)
		}
		.with_guessed_format().unwrap()
		.decode().unwrap()
		.into_rgba8()
	;
	
	validate_metatiles_image(&image_data).unwrap();

	image_data
}


impl Color_context {
	
	pub fn process_metatile_image(&mut self, 
		image: &RgbaImage, 
		tile_result: &mut BTreeSet<crate::tileset::Shape_indexable_tile>, 
		color_result: &mut BTreeSet<Indexed_color_set>
	) -> Vec<Tile_instance_intermediate> {
		let width = image.width() as usize;
		assert!(width % 16 == 0);
		let height = image.height() as usize;
		assert!(height % 16 == 0);
		let metatile_x_count = width / 16;
		let metatile_y_count = height / 16;
		let expect_count: usize = metatile_x_count * metatile_y_count;
		
		let mut layer_mapping = Vec::<Tile_instance_intermediate>::new();

		for metatile_index in 0..expect_count {
			let mt_x: usize = (metatile_index % metatile_x_count) * 16;
			let mt_y: usize = (metatile_index / metatile_x_count) * 16;


			for subtile_index in 0..4 {
				let st_x: usize = (subtile_index % 2) * 8;
				let st_y: usize = (subtile_index / 2) * 8;

				let mut mask_set = BTreeMap::<Color_index, Tile_mask>::new();
				let mut color_set = Indexed_color_set::new();

				for tile_pixel in 0..64 {
					let tp_x: usize = tile_pixel % 8;
					let tp_y: usize = tile_pixel / 8;

					let x = mt_x + st_x + tp_x;
					let y = mt_y + st_y + tp_y;
					
					let pixel = image.get_pixel(x as u32, y as u32).0.into();
					if let Some(color) = Color24::from_rgba(pixel) {
						let color_index = self.insert_get_index(color);
						
						if None == mask_set.get_mut(&color_index) {
							mask_set.insert(color_index, Tile_mask::new());
							color_set.set_bit(color_index);
						}
						
						let mask = mask_set.get_mut(&color_index).unwrap();
						mask.rows[tp_y] |= 1 << tp_x;
					}
				}
				let reversed_key_val = mask_set.into_iter().map(|(color,mask)|(mask,color));
				let indexable_tile = Shape_indexable_tile{colors:BTreeMap::from_iter(reversed_key_val)};
				let ideal_flip = indexable_tile.get_ideal_flip();
				tile_result.insert(ideal_flip.shape.clone());
				color_result.insert(color_set);
				layer_mapping.push(ideal_flip);
			}
		}
		layer_mapping
	}
}


fn interleave_metatiles(metatile_maps: Vec<Vec<Tile_instance_intermediate>>) -> Metatile_interleaver {
	Metatile_interleaver{metatiles:metatile_maps, index:0}
}


struct Metatile_interleaver
{
	metatiles: Vec<Vec<Tile_instance_intermediate>>,
	index: usize
}


impl Iterator for Metatile_interleaver
{
	type Item = Tile_instance_intermediate;

	fn next(&mut self) -> Option<Self::Item> {
		let index = self.index;
		let  quad = index % 4;
		let index = index / 4;
		let layer_index = index % self.metatiles.len();
		let index = index / self.metatiles.len();

		let layer = &self.metatiles[layer_index];
		let res = layer.get(index*4 + quad);
		if res == None {
			return None;
		}
		self.index += 1;
		return res.cloned();
	}
}


pub fn assign_palettes(
  tiles: BTreeSet<Shape_indexable_tile>, 
  basic_palettes: BTreeSet<Indexed_color_set>,
  palette_list: Palette_list,
) -> Vec<(Indexed_color_set, [Option<Color_index>; 16])> {
  let mut palette_list = palette_list;


  for p in basic_palettes {
    palette_list.insert_ics(p);
  }


	for (a, Palette_info(b, _)) in palette_list.0.iter() {
		let b_set = b.keys().cloned().collect();
		assert_eq!(*a, b_set);
	}

	
  account_for_palette_swaps(tiles, &mut palette_list);
	let palette_count = palette_list.0.len();

	// Get rid of undetermined positions
	for (_, Palette_info(color_position_map, _)) in palette_list.0.iter_mut() {
		let mut used_pos = std::collections::HashSet::new();
		let mut i = 1;
		for (_, position) in color_position_map.iter_mut() {
			use palette::Position_in_palette::*;
			match *position {
				Undetermined => {},
				Absolute(p) => {used_pos.insert(p);},
				Indirect(_, _) => {},
			}
		}
		for (_, position) in color_position_map.iter_mut() {
			use palette::Position_in_palette::*;
			while used_pos.contains(&i) {i += 1}
			match *position {
				Undetermined => {*position = Absolute(i); i += 1;},
				Absolute(_) => {},
				Indirect(_, _) => {},
			}
		}
	};
	
	for (a, Palette_info(b, _)) in palette_list.0.iter() {
		let b_set = b.keys().cloned().collect();
		assert_eq!(*a, b_set);
	}

	// Get rid of indirect positions
	let mut palette_list_2 = Vec::new();
	for (color_set, Palette_info(color_position_map, palette_position)) in palette_list.0.iter() {
		let mut palette = [None; 16];

		// color_position_map

		for (color_index, position) in color_position_map.iter() {
			use crate::palette::Position_in_palette::*;
			
			let mut position = position.clone();
			// Loop navigates the indirection with a max iter.
			for _ in 0..palette_count { 
				match position {
					Undetermined => {panic!("All undetermined must be worked out before this point");},
					Absolute(_) => {break;},
					Indirect(palette_index, color_index) => {
						// palette_list_2[palette_index.0]
						let (_, Palette_info(color_position_map, _)) = &palette_list.0[palette_index.0 as usize];
						position = color_position_map[&color_index];
					},
				}
			}
			if let Absolute(position) = position {
				assert!(position != 0);
				palette[position as usize] = Some(color_index.clone());
			} else {
				// If the loop runs out its a cycle and needs to be nuked
				panic!("Cycle detected");
			}
		}
		palette_list_2.push((*color_set, palette, *palette_position));
	}


	drop(palette_list);

	// Determine absolute palette positions
	let mut palette_list_3 = Vec::new();
	{
		let mut used_palette_pos = std::collections::HashSet::new();
		for (_,_, palette_position) in palette_list_2.iter() {
			if let Some(pos) = palette_position {
				used_palette_pos.insert(pos.0);
			}
		}
		let mut i = 0;
		for (palette_set,palette, palette_position) in palette_list_2 {
			let res;
			if let Some(pos) = palette_position {
				res = pos;
			}
			else {
				while used_palette_pos.contains(&i) {i += 1}
				res = Palette_index(i);
				i += 1;
			}
	
			growable_vec_insert(
				&mut palette_list_3, res.0 as _, 
				||(palette_set, palette), 
				||(Indexed_color_set::new(),[None; 16])
			);
		}
	}
	// palette_list_3


	// for (a,b) in palette_list_3.iter() {
	// 	let b_set = b.iter().cloned().collect();
	// 	assert_eq!(*a, b_set);
	// }

	palette_list_3
}


fn account_for_palette_swaps(
  tiles: BTreeSet<Shape_indexable_tile>, 
  palette_list: &mut Palette_list,
) -> () {
  
  let mut shape_color_seq_mapping = BTreeMap::<Shape, BTreeMap::<Palette_index, BTreeSet<Color_sequence>>>::new();
  
  for tile in tiles {

    let color_sequence: Color_sequence = tile.colors.values().cloned().collect();
    let mut pal = Palette_index(0);
    let this_palette = color_sequence.iter().cloned().collect();
    for (palette, _palette_info) in palette_list.0.iter() {
      if palette.is_superset_of(&this_palette) {
        break;
      }
      pal.0 += 1;
    }

    let shape: Shape = tile.colors.keys().cloned().collect();

    if let Some(palette_to_color_mapping) = shape_color_seq_mapping.get_mut(&shape) {
			
      palette_to_color_mapping.bucket_insert(pal, color_sequence);
    }
    else {
      let mut bucket_b = BTreeMap::<Palette_index, BTreeSet<Color_sequence>>::new();
      bucket_b.bucket_insert(pal, color_sequence);
      shape_color_seq_mapping.insert(shape, bucket_b);
    }
  } 

	/*
		At first it might seem that what were about to do should have been 
		done above, but I believe restructuring it first saves us from some
		cycle bugs. 
	*/


  for (_shape, mut palette_mapping) in shape_color_seq_mapping {
    if palette_mapping.len() > 1 {
      
      // Note here that we're only taking the first color set of each shape and of each palette
      let mut palette_mapping_iter = palette_mapping.iter_mut();
      let ( pal_a, pal_a_sets ) = palette_mapping_iter.next().unwrap();

      // Iterate over the palette swaps of the first one
      for ( pal_b, pal_b_sets ) in palette_mapping_iter {
        
        // Match up the color sets from the two palettes being compared
        for (set_a, set_b) in pal_a_sets.iter().zip(pal_b_sets.iter()) {
          let (_, Palette_info(palette_b, _)) = &mut palette_list[*pal_b];

          assert_eq!(set_a.len(), set_b.len());

          for (color_a, color_b) in set_a.iter().zip(set_b.iter()) {
            // closure(*pal_a, *pal_b, *color_a, *color_b);
            use crate::palette::Position_in_palette::*;
            // The palettes may get reordered so these need to be kept in sync
            match palette_b[&color_b] {
              Undetermined => {palette_b.insert(*color_b, Indirect(*pal_a, *color_a));},
              Absolute(_) => {},
              Indirect(_, _) => {},
            }
          }
        }
      } 
    }
  }
}
