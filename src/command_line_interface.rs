use clap::{Parser, Subcommand};

use crate::decompilation::decompile_primary;
use crate::{decompilation, helpers::*};
use crate::compilation::compile_primary;

/// Option for normalizing transparent colors before processing


pub fn handle_arguments() {
	let cli = Command_line_interface::parse();

	match cli {
		Command_line_interface::compile(compile) => compile.go(),
		Command_line_interface::decompile(decompile) => decompile.go(),
	}
}




#[derive(Debug, Parser)]
#[command(about = "Convert images into tiles")]
enum Command_line_interface {
	#[command(subcommand)]
	compile(Compile_tileset),

	#[command(subcommand)]
	decompile(Decompile_tileset),

}




#[derive(Debug, Subcommand)]
enum Compile_tileset {
	primary{
		name: String,
		// layer_name_pattern: Option<String>,
		layer_names: String,
		max_palette_count: Option<usize>,
		max_palette_size: Option<usize>,
	},
	secondary{
		name: String,
		// layer_name_pattern: Option<String>,
		layer_names: String,
		max_palette_count: Option<usize>,
		max_palette_size: Option<usize>,
		
		primary_name: String,
	}
}


#[derive(Debug, Subcommand)]
enum Decompile_tileset {
	primary{
		name: String,
		layer_count: usize,
		output_layer_names: String,
	},
	secondary{
		name: String,
		layer_count: usize,
		output_layer_names: String,
		
		primary_name: String,
	}
}


impl Decompile_tileset {
	fn go(self) {
		match self {
			Decompile_tileset::primary { 
				name, 
				layer_count, 
				output_layer_names 
			} => {
				let path = get_primary_path().join(name);
				let images = decompile_primary(&path, layer_count);
				let output_layer_names = split_layer_names(&output_layer_names);
				let output_path = path.join("decompiled_tileset");
				ascertain_directory_exists(&output_path).unwrap();
				for i in 0..layer_count {
					images[i].save_with_format(
						output_path.join(output_layer_names[i]), 
						image::ImageFormat::Png).unwrap();
				}
			},
			Decompile_tileset::secondary { 
				name, 
				layer_count, 
				output_layer_names, 
				primary_name 
			} => todo!(),
		}
	}
}

impl Compile_tileset {
	fn go(self) {
		let primary_path = get_primary_path();
		let secondary_path = get_secondary_path();

		match self {
			Compile_tileset::primary { 
				name, 
				max_palette_count,
				// layer_name_pattern, 
				layer_names, 

				max_palette_size, 
			} => {
				let max_palette_size = max_palette_size.unwrap_or(16);
				let max_palette_count = max_palette_count.unwrap_or(6);
				let path = primary_path.join(name);
				let layer_names = split_layer_names(&layer_names);
				compile_primary(&path, &layer_names).unwrap();
			},
			Compile_tileset::secondary { 
				name, 
				// layer_name_pattern, 
				layer_names, 
				max_palette_count, 
				max_palette_size,

				primary_name, 
			} => {
				let max_palette_size = max_palette_size.unwrap_or(16);
				// let primary_max_palette_count = primary_max_palette_count.unwrap_or(6);
				let max_palette_count = max_palette_count.unwrap_or(7);

				let primary_path = primary_path.join(primary_name);

				let primary_tiles = decompilation::load_tileset(&primary_path);
				let primary_palettes = decompilation::load_palette_folder(&primary_path, max_palette_size, 6);

				let path = secondary_path.join(name);
				let layer_names = split_layer_names(&layer_names);
			}
		}
	}

}


fn split_layer_names(string: &str) -> Vec<&str> {
	string.split(",").map(|s|s.trim()).collect()
}