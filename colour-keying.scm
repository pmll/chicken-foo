;; demo colour keying - i.e. choose a colour to be transparent in an image
(import (prefix sdl2 "sdl2:")
        (prefix sdl2-image "img:")
        (chicken condition))

(define screen-width 640)
(define screen-height 480)

;; Initialise SDL
(sdl2:set-main-ready!)
(sdl2:init! '(video) '(events))

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

(define (make-ltexture file)
  (let ((file-surface (img:load file)))
    (set! (sdl2:surface-colour-key file-surface)
          (sdl2:map-rgb (sdl2:surface-format file-surface) #x0 #xFF #xFF))
    (list (sdl2:create-texture-from-surface screen-renderer file-surface)
          (sdl2:surface-w file-surface)
          (sdl2:surface-h file-surface))))

(define (render-ltexture! ltexture x y)
  (sdl2:render-copy! screen-renderer
                     (car ltexture)
                     #f
                     (sdl2:make-rect x y (cadr ltexture) (caddr ltexture))))

(define foo-texture (make-ltexture "10_color_keying/foo.png"))

(define background-texture (make-ltexture "10_color_keying/background.png"))

(define (while-not-quit proc)
  (when (not (sdl2:quit-requested?))
    (begin
      (proc)
      (while-not-quit proc))))

(while-not-quit
  (lambda ()
    (begin
      (set! (sdl2:render-draw-colour screen-renderer)
            (sdl2:make-colour #xFF #xFF #xFF #xFF))
      (sdl2:render-clear! screen-renderer)
      (render-ltexture! background-texture 0 0)
      (render-ltexture! foo-texture 240 190)
      (sdl2:render-present! screen-renderer))))

