
(defpackage :glsl
  (:import-from #:ppcre #:regex-replace-all)
  (:use #:cl #:alexandria #:split-sequence)
  (:export ;;#:defpipeline
	   #:v-defmacro
	   #:discard
	   #:lisp
	   #:true
	   #:false
	   #:initialize
	   #:s~
	   #:multf
	   #:cond!
	   #:if!
	   #:gl-point-size
	   #:gl-vertex-id
	   #:gl-instance-id
	   #:gl-frag-coord
	   #:gl-point-coord
	   #:gl-frag-depth
	   #:gl-depth-range
	   #:gl-position))

(defpackage :gfx
  (:use #:cl #:glsl)
  (:export #:deg-to-rad
	   #:get-internal-seconds

	   #:shader-environment
	   #:release-environment
	   
	   #:make-gpu-stream
	   #:gpu-stream-set
	   #:gpu-stream-length
	   #:gpu-stream-do-each
	   
	   #:make-shader-object
	   #:with-shader
	   #:set-uniform
	   
	   #:pull-g
	   #:defun-g
	   #:defvar-g
	   #:defstruct-g
	   #:defmacro-g

	   #:reinit-shader-system
	   #:defpipeline

	   #:make-fbo
	   #:reinit-fbo
	   #:width
	   #:height
	   #:output-texture 
	   #:release-fbo
	   #:with-fbo
	   #:*fbo-stack*
	   
	   #:camera
	   #:track-mouse-spin
	   #:track-mouse-zoom
	   #:track-mouse-pan
	   #:reset-camera
	   #:eval-camera
	   #:read-obj-file

	   #:load-quad-stream))

