(defstruct ppu
  cartridge
  (name-table (bytememory 2048))
  (pallete-table (bytememory 32)))
