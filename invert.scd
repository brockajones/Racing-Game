(module invert (invert-texture invert-renderer) 
(import scheme)
(import chicken)
(import foreign)
(use lolevel)



;; Copyright © 2013, 2015-2016  John Croisant.
;; All rights reserved.

(define (int-guard description #!key min max)
  (cond
   ((and min max)
    (lambda (value)
      (assert (and (integer? value) (<= min value max))
              (format "~A must be an integer in range [~S, ~S], but got: ~S"
                      description min max value))
      (inexact->exact value)))
   (min
    (lambda (value)
      (assert (and (integer? value) (<= min value))
              (format "~A must be an integer >= ~S, but got: ~S"
                      description min value))
      (inexact->exact value)))
   (max
    (lambda (value)
      (assert (and (integer? value) (<= value max))
              (format "~A must be an integer <= ~S, but got: ~S"
                      description max value))
      (inexact->exact value)))
   (else
    (lambda (value)
      (assert (integer? value)
              (format "~A must be an integer, but got: ~S"
                      description value))
      (inexact->exact value)))))

(define (Uint-guard description bits)
  (int-guard description
             min: 0
             max: (- (expt 2 bits) 1)))

(define (Uint8-guard  description) (Uint-guard description 8))
(define (Uint16-guard description) (Uint-guard description 16))
(define (Uint32-guard description) (Uint-guard description 32))
(define (Uint64-guard description) (Uint-guard description 64))


(define (Sint-guard description bits)
  (int-guard description
             min: (- (expt 2 (- bits 1)))
             max: (- (expt 2 (- bits 1)) 1)))

(define (Sint8-guard  description) (Sint-guard description 8))
(define (Sint16-guard description) (Sint-guard description 16))
(define (Sint32-guard description) (Sint-guard description 32))
(define (Sint64-guard description) (Sint-guard description 64))

(define-syntax define-struct-record-type
  (syntax-rules (wrap: unwrap: pred:)
    ((define-struct-record-type
       record-name struct-name-str
       pred:    pred?
       wrap:    wrapper
       unwrap:  unwrapper
       (pointer pointer-getter
                pointer-setter!))
     (begin
       (define-record-type record-name
         (wrapper pointer)
         pred?
         (pointer pointer-getter pointer-setter!))
       (define-struct-record-unwrapper unwrapper
         struct-name-str pred? pointer-getter)))))

(define-syntax define-struct-record-unwrapper
  (syntax-rules ()
    ((define-struct-record-unwrapper unwrapper
       struct-name record-pred? record-pointer)
     (define (unwrapper thing)
       (cond
        ((record-pred? thing)
         (unwrapper (record-pointer thing)))
        ((blob? thing)
         (make-locative thing))
        ((or (pointer? thing) (locative? thing) (not thing))
         thing)
        (else
         (error (format "~S cannot convert ~S to ~A pointer"
                        'unwrapper thing struct-name))))))))

(define-syntax define-struct-record-printer
  (syntax-rules (show-address:)
    ((define-struct-record-printer record-type
       record-pointer
       show-address: show-address?
       (field-label field-getter)
       ...)
     (define-record-printer (record-type record out)
       (display (sprintf "#<~A" 'record-type) out)
       (let ((address (%address #f (record-pointer record))))
         (if (= 0 address)
             (display " NULL" out)
             (begin
               (macro-when show-address?
                 (display (sprintf " 0x~X" address) out))
               (begin
                 (macro-when field-label
                   (display (sprintf " ~A:" 'field-label) out))
                 (display (sprintf " ~S" (field-getter record)) out))
               ...)))
       (display ">" out)))))

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

(define-struct-record-type
  sdl2:renderer "SDL_Renderer"
  pred:    renderer?
  wrap:    wrap-renderer
  unwrap:  unwrap-renderer
  (pointer %renderer-pointer
           %renderer-pointer-set!))

(define-struct-record-printer sdl2:renderer
  %renderer-pointer
  show-address: #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERIC

(define-foreign-type float*  (c-pointer float))

(define-foreign-type Sint8   byte       (Sint8-guard  "Sint8 value"))
(define-foreign-type Sint16  short      (Sint16-guard "Sint16 value"))
(define-foreign-type Sint32  integer32  (Sint32-guard "Sint32 value"))
(define-foreign-type Sint64  integer64  (Sint64-guard "Sint64 value"))

(define-foreign-type Sint8*  (c-pointer byte))
(define-foreign-type Sint16* (c-pointer short))
(define-foreign-type Sint32* (c-pointer integer32))
(define-foreign-type Sint64* (c-pointer integer64))

(define-foreign-type Uint8   unsigned-byte       (Uint8-guard  "Uint8 value"))
(define-foreign-type Uint16  unsigned-short      (Uint16-guard "Uint16 value"))
(define-foreign-type Uint32  unsigned-integer32  (Uint32-guard "Uint32 value"))
(define-foreign-type Uint64  unsigned-integer64  (Uint64-guard "Uint64 value"))

(define-foreign-type Uint8*  (c-pointer unsigned-byte))
(define-foreign-type Uint16* (c-pointer unsigned-short))
(define-foreign-type Uint32* (c-pointer unsigned-integer32))
(define-foreign-type Uint64* (c-pointer unsigned-integer64))

(define-foreign-type SDL_Texture*
  (nonnull-c-pointer "SDL_Texture")
  unwrap-texture
  wrap-texture)

(define-foreign-type SDL_Texture*-or-null
  (c-pointer "SDL_Texture")
  unwrap-texture
  wrap-texture)

(define-foreign-type SDL_Renderer*
  (nonnull-c-pointer "SDL_Renderer")
  unwrap-renderer
  wrap-renderer)

(define (%address fn obj)
  (cond ((record-instance? obj)
         (%address fn (record-instance-slot obj 0)))
        ((or (pointer? obj) (locative? obj))
         (pointer->address obj))
        ((blob? obj)
         (pointer->address (make-locative obj)))
        ((not obj)
         0)
        (else
         (error fn "Unsupported object" obj))))

(define-syntax macro-when
  (syntax-rules ()
    ((macro-when x expr ...)
     (macro-if x (begin expr ...) (void)))))


	;(include "library/guard.scm")
	;(include "library/struct.scm")
	;(include "library/record-types/texture.scm")
	;(include "library/record-types/renderer.scm")
	;(include "library/foreign-types.scm")


	;Everything below this point is Brock's code and not copyright of John Croisant.


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
	  "SDL_SetRenderDrawBlendMode(renderer, negative);")))
