;; demo key states
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

(define (render-ltexture! ltexture x y clip)
  (sdl2:render-copy!
    screen-renderer
    (car ltexture)
    clip
    (sdl2:make-rect x
                    y
                    (if clip (sdl2:rect-w clip) (cadr ltexture))
                    (if clip (sdl2:rect-h clip) (caddr ltexture)))))

(define press-texture (make-ltexture "18_key_states/press.png"))

(define up-texture (make-ltexture "18_key_states/up.png"))

(define down-texture (make-ltexture "18_key_states/down.png"))

(define left-texture (make-ltexture "18_key_states/left.png"))

(define right-texture (make-ltexture "18_key_states/right.png"))

(define (key-state-texture)
  (cond ((sdl2:scancode-pressed? 'up) up-texture)
        ((sdl2:scancode-pressed? 'down) down-texture)
        ((sdl2:scancode-pressed? 'left) left-texture)
        ((sdl2:scancode-pressed? 'right) right-texture)
        (else press-texture)))

(define (while-not-quit proc)
  (when (not (sdl2:quit-requested?))
     (begin
       (proc)
       (while-not-quit proc))))

(while-not-quit
  (lambda () 
    (begin
      (sdl2:get-events! 999)
      (set! (sdl2:render-draw-colour screen-renderer)
            (sdl2:make-colour #xFF #xFF #xFF #xFF))
      (sdl2:render-clear! screen-renderer)
      (render-ltexture! (key-state-texture) 0 0 #f)
      (sdl2:render-present! screen-renderer))))

