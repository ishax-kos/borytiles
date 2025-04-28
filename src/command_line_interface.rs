use std::env::current_dir;

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
		#[arg(long="root")]
		project_root: Option<String>,
		name: String,
		// layer_name_pattern: Option<String>,
		layer_names: String,
		max_palette_count: Option<usize>,
		max_palette_size: Option<usize>,
	},
	secondary{
		#[arg(long="root")]
		project_root: Option<String>,
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
		#[arg(long="root")]
		project_root: Option<String>,
		name: String,
		output_layer_names: String,
	},
	secondary{
		#[arg(long="root")]
		project_root: Option<String>,
		name: String,
		output_layer_names: String,
		
		primary_name: String,
	}
}


impl Decompile_tileset {
	fn go(self) {
		match self {
			Decompile_tileset::primary {
				project_root,
				name, 
				output_layer_names 
			} => {
				let path = determine_project_root(project_root).unwrap()
					.join(raw_primary_path)
					.join(name);
				let output_layer_names = split_layer_names(&output_layer_names);
				
				decompile_primary(&path, &output_layer_names);

			},
			Decompile_tileset::secondary { 
				project_root,
				name, 
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
				project_root, 
				name, 
				max_palette_count,
				// layer_name_pattern, 
				layer_names, 

				max_palette_size, 
			} => {
				let path = determine_project_root(project_root).unwrap()
					.join(raw_primary_path)
					.join(name);
				let max_palette_size = max_palette_size.unwrap_or(16);
				let max_palette_count = max_palette_count.unwrap_or(6);
				let layer_names = split_layer_names(&layer_names);
				compile_primary(&path, &layer_names).unwrap();
			},
			Compile_tileset::secondary { 
				project_root,
				name, 
				// layer_name_pattern, 
				layer_names, 
				max_palette_count, 
				max_palette_size,

				primary_name, 
			} => {
				let project_root = determine_project_root(project_root).unwrap();
				let path = project_root
					.join(raw_secondary_path)
					.join(&name);

				let max_palette_size = max_palette_size.unwrap_or(16);
				let max_palette_count = max_palette_count.unwrap_or(7);

				let primary_path = project_root
					.join(primary_path)
					.join(primary_name);

				let primary_tiles = decompilation::load_tiles_png(&primary_path);

				let layer_names = split_layer_names(&layer_names);
			}
		}
	}
}


fn split_layer_names(string: &str) -> Vec<&str> {
	string.split(",").map(|s|s.trim()).collect()
}


fn determine_project_root(root_arg: Option<String>) -> Result<std::path::PathBuf, std::io::Error> {
	if let Some(root_arg) = root_arg {
		PathBuf::from(root_arg).canonicalize()
	}
	else {
		current_dir()
	}
}