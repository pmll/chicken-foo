;; demo rendering fonts
(import (prefix sdl2 "sdl2:")
        (prefix sdl2-image "img:")
        (prefix sdl2-ttf "ttf:")
        (chicken condition))

(define screen-width 640)
(define screen-height 480)

;; Initialise SDL
(sdl2:set-main-ready!)
(sdl2:init! '(video) '(events))
(ttf:init!)

;; Schedule quit! to be automatically called on normal exit
(on-exit sdl2:quit!)

;; Install a custom exception handler that will call quit! then call the
;; original exception handler. This will insure that quit! is called even
;; if an unhandled exception reaches the top level
(current-exception-handler
 (let ((original-handler (current-exception-handler)))
   (lambda (exception)
     (sdl2:quit!)
     (original-handler exception))))

(define window
  (sdl2:create-window! "SDL Tutorial" 0 0 screen-width screen-height))

(define screen-renderer (sdl2:create-renderer! window -1 '(accelerated)))

(define font (ttf:open-font "16_true_type_fonts/lazy.ttf" 28))

(define (make-ltexture file)
  (let ((file-surface (img:load file)))
    (set! (sdl2:surface-colour-key file-surface)
          (sdl2:map-rgb (sdl2:surface-format file-surface) #x0 #xFF #xFF))
    (list (sdl2:create-texture-from-surface screen-renderer file-surface)
          (sdl2:surface-w file-surface)
          (sdl2:surface-h file-surface))))

(define (make-ltexture-from-text text colour)
  (let ((text-surface (ttf:render-text-solid font text colour)))
    (list (sdl2:create-texture-from-surface screen-renderer text-surface)
          (sdl2:surface-w text-surface) 
          (sdl2:surface-h text-surface))))

(define (ltexture-w ltexture) (cadr ltexture))

(define (ltexture-h ltexture) (caddr ltexture))

(define (render-ltexture! ltexture x y clip)
  (sdl2:render-copy!
    screen-renderer
    (car ltexture)
    clip
    (sdl2:make-rect x
                    y
                    (if clip (sdl2:rect-w clip) (cadr ltexture))
                    (if clip (sdl2:rect-h clip) (caddr ltexture)))))

(define text-texture
  (make-ltexture-from-text "The quick brown fox jumps over the lazy dog"
                           (sdl2:make-colour 0 0 0)))

(define (while-not-quit proc)
  (when (not (sdl2:quit-event? (sdl2:poll-event!)))
    (begin
      (proc)
      (while-not-quit proc))))

(while-not-quit
  (lambda ()
    (set! (sdl2:render-draw-colour screen-renderer)
          (sdl2:make-colour #xFF #xFF #xFF #xFF))
    (sdl2:render-clear! screen-renderer)
    (render-ltexture! text-texture 
                      (quotient (- screen-width (ltexture-w text-texture)) 2)
                      (quotient (- screen-height (ltexture-h text-texture)) 2)
                      #f)
    (sdl2:render-present! screen-renderer)))

