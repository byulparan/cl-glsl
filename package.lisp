
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

	   #:shader
	   #:vbo
	   #:shader-environment
	   #:release-environment

	   #:update-gpu-stream
	   #:make-gpu-stream
	   #:gpu-stream-set
	   #:gpu-stream-length
	   #:vertex 
	   
	   #:make-shader-object
	   #:with-shader
	   #:set-uniform
	   
	   #:pull-g
	   #:defun-g
	   #:defvar-g
	   #:defstruct-g
	   #:defmacro-g

	   #:add-uniform
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
	   #:eye-x
	   #:eye-y
	   #:eye-z
	   #:center-x
	   #:center-y
	   #:center-z
	   #:up-x
	   #:up-y
	   #:up-z

	   #:read-obj-file
	   #:load-box-primitive

	   #:load-model
	   #:model-scene
	   #:model-meshes
	   #:mesh-gpu-stream
	   #:mesh-textures
	   #:mesh-bones
	   #:mesh-transform
	   #:mesh-bone-transforms
	   #:animation))

