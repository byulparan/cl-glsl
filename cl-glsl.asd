(asdf:defsystem :cl-glsl
  :serial t
  :depends-on (#:cl-opengl
	       #:classimp
	       #:cl-ppcre
	       #:split-sequence
	       #:sb-cga
	       #:mathkit)
  :components ((:file "package")
	       (:file "type")
	       (:file "compiler")
	       (:file "variables")
	       (:file "functions")
	       (:file "macros")
	       (:file "shader")
	       (:file "gfx/interface")
	       (:file "gfx/shader-environment")
	       (:file "gfx/gfx")
	       (:file "gfx/shader-object")
	       (:file "gfx/fbo")
	       (:file "gfx/camera")
	       (:file "gfx/read-wavefront")
	       (:file "gfx/primitives")
	       (:file "gfx/model")
	       (:file "gfx/shader-library")))
