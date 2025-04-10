pub mod palette;
pub mod tileset;



use image::*;

use crate::{compilation::palette::*, helpers::*};
use std::{collections::BTreeSet, fmt::Debug};


#[derive(Debug)]
struct Err_invalid_image(String);


fn validate_metatiles_image(image: &ImageBuffer<Rgba<u8>, Vec<u8>>) -> Result<(), Vec<Err_invalid_image>> {
	let mut errors = vec![];
	if image.width() % 16 != 0 {
		errors.push(Err_invalid_image("Width is not a multiple of 16.".into()));
	}
	if image.height() % 16 != 0 {
		errors.push(Err_invalid_image("Height is not a multiple of 16.".into()));
	}

	if errors.is_empty() {
		return Ok(());
	}
	else {
		return Err(errors);
	}
}


fn validate_tiles_image(image: ImageBuffer<Rgba<u8>, Vec<u8>>) -> Result<(), Vec<Err_invalid_image>> {
	let mut errors = vec![];
	if image.width() % 8 != 0 {
		errors.push(Err_invalid_image("Width is not a multiple of 8.".into()));
	}
	if image.height() % 8 != 0 {
		errors.push(Err_invalid_image("Height is not a multiple of 8.".into()));
	}

	if errors.is_empty() {
		return Ok(());
	}
	else {
		return Err(errors);
	}
}


fn get_image_colors(images: &Vec<RgbaImage>) -> BTreeMap<Gba_color, BTreeSet<[u8; 4]>> {
	let mut palette = BTreeMap::<Gba_color, BTreeSet<_>>::new();
	
	for image in images {
		for y in 0..image.height() {
			for x in 0..image.width() {
				// In the future we can store the position where each Color32 was 
				// first found in the image which is helpful for debugging
				let pixel = image.get_pixel(x, y).0;
				let color = Color24::from_rgba(pixel);
				palette.bucket_insert(Gba_color::from(color), pixel);
			}
		}
	}
	palette
}



pub fn compile_primary(tileset_path: &Path, layer_names: &[&str]) -> Result<(),()> {
	let primary_path = get_primary_path();

	let images: Vec<_> = layer_names.into_iter()
		.map(|name| open_image(&tileset_path.join(name)))
		.collect();

	let mut color_context = Color_context::new();
	let mut tiles = BTreeSet::new();
	let mut colors = BTreeSet::new();

	for layer_image in images {
		color_context.sequence_tiles(&layer_image, &mut tiles, &mut colors);
	}

	let colors = filter_subsets(&colors);
	let colors = condense_palettes_by_overlap(colors);

	Ok(())
}


fn show_user_unmappable_tiles(tiles: &Vec<SubImage<&RgbaImage>>, palettes_mapping: &Vec<Option<u8>>) {
	todo!()
}


pub fn open_image(path: &Path) -> RgbaImage {
	let image_data = 
	match ImageReader::open(&path) {
		Ok(result) => result,
		Err(e) => panic!("{}\n{}", path.display(), e)
	}
		.with_guessed_format().unwrap()
		.decode().unwrap()
		.into_rgba8();
	
	validate_metatiles_image(&image_data).unwrap();

	image_data
}