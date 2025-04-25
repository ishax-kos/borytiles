


#[repr(u8)]
enum Layer_configuration {
	normal = 0,
	covered = 1,
	split = 2
}

struct Metatile_attribute {
	behavior: u8,
	layers: Layer_configuration,
}



// Bit   Expl.
// 0-9   Tile Number     (0-1023) (a bit less in 256 color mode, because
// 												 there'd be otherwise no room for the bg map)
// 10    Horizontal Flip (0=Normal, 1=Mirrored)
// 11    Vertical Flip   (0=Normal, 1=Mirrored)
// 12-15 Palette Number  (0-15)    (Not used in 256 color/1 palette mode)