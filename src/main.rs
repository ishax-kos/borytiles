#![allow(non_camel_case_types)]
#![allow(non_upper_case_globals)]
#![deny(unused_must_use)]

mod output;
mod compilation;
mod helpers;
mod bucket;
mod decompilation;
mod command_line_interface;
mod palette;
mod tileset;

fn main() {
	command_line_interface::handle_arguments()
}
