;;;; nes.asd

(asdf:defsystem #:nes
  :description "NES emulator written in lisp"
  :author "santi leonardors@dcc.ufrj.br"
  :license  "idk"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-6502)
  :components ((:file "package")
	       (:file "src/utils")
	       (:file "src/mapper")
	       (:file "src/cartridge")
	       (:file "src/ppu")
	       (:file "src/bus")
               (:file "nes")))
