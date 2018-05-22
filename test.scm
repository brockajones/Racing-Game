(use sdl2)
(import foreign)
(use lolevel)

(include "library/guards.scm")
(include "library/foreign-types.scm")
(include "library/struct.scm")

(foreign-declare "#include<SDL2/SDL.h>")

(define-struct-record-type
  sdl2:texture "SDL_Texture"
  pred:    texture?
  wrap:    wrap-texture
  unwrap:  unwrap-texture
  (pointer %texture-pointer
	   %texture-pointer-set!))

(define-struct-record-printer sdl2:texture
			      %texture-pointer
			      show-address: #t)


(define-type sdl2:texture       (struct sdl2:texture))
(define-type sdl2:texture*       (or pointer locative sdl2:texture))

(define-foreign-type Sint32  integer32  (Sint32-guard "Sint32 value"))

(define-foreign-type Uint8   unsigned-byte       (Uint8-guard  "Uint8 value"))

(define-foreign-type SDL_Texture*
		       (nonnull-c-pointer "SDL_Texture")
		         unwrap-texture
			   wrap-texture)

(define-syntax define-function-binding
  (syntax-rules (return: args:)
		;; return type and args
		((define-function-binding func-name-symbol
					  return: (return-type return-semantics)
					  args: ((arg-type arg-name) ...))
		 (define func-name-symbol
		   (foreign-lambda return-type func-name-symbol arg-type ...)))
		;; no args
		((define-function-binding func-name-symbol
					  return: (return-type return-semantics))
		 (define func-name-symbol
		   (foreign-lambda return-type func-name-symbol)))
		;; no return type (i.e. void)
		((define-function-binding func-name-symbol
					  args: ((arg-type arg-name) ...))
		 (define func-name-symbol
		   (foreign-lambda void func-name-symbol arg-type ...)))
		;; no return type or args
		((define-function-binding func-name-symbol)
		 (define func-name-symbol
		   (foreign-lambda void func-name-symbol)))))

(define-function-binding SDL_SetTextureAlphaMod
			 return: (Sint32 zero-on-success)
			 args: ((SDL_Texture* texture)
				(Uint8        alpha)))

(define dim
  (foreign-lambda* void ((SDL_Texture* texture))
		   "SDL_BlendMode negative = SDL_ComposeCustomBlendMode(
			   SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR, 
		   SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA, 
		   SDL_BLENDOPERATION_ADD, 
		   SDL_BLENDFACTOR_ZERO, 
		   SDL_BLENDFACTOR_ZERO, 
		   SDL_BLENDOPERATION_ADD);"
		   "SDL_SetTextureBlendMode(texture,negative);"))
