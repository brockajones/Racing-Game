(import foreign)
(use lolevel)

	(include "library/guards.scm")
	(include "library/struct.scm")
	(include "library/record-types/texture.scm")
	(include "library/record-types/texture.scm")
	(include "library/record-types/renderer.scm")
	(include "library/foreign-types.scm")

	(foreign-declare "#include<SDL2/SDL.h>")


	(define invert-texture
	 (lambda (texture) 
	 ((foreign-lambda* void ((SDL_Texture* texture))
	  "SDL_BlendMode negative = SDL_ComposeCustomBlendMode(
		  SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR, 
		  SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA, 
		  SDL_BLENDOPERATION_ADD, 
		  SDL_BLENDFACTOR_ZERO, 
		  SDL_BLENDFACTOR_ZERO, 
		  SDL_BLENDOPERATION_ADD);"
	  "SDL_SetTextureBlendMode(texture,negative);") texture)
	 texture))

	(define invert-renderer
	 (foreign-lambda* void ((SDL_Renderer* renderer))
	  "SDL_BlendMode negative = SDL_ComposeCustomBlendMode(
		  SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR, 
		  SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA, 
		  SDL_BLENDOPERATION_ADD, 
		  SDL_BLENDFACTOR_ZERO, 
		  SDL_BLENDFACTOR_ZERO, 
		  SDL_BLENDOPERATION_ADD);"
	  "SDL_SetRenderDrawBlendMode(renderer, negative);"))

