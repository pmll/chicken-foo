;; demo colour modulation
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

(define (ltexture-set-colour! ltexture r g b)
  (set! (sdl2:texture-colour-mod (car ltexture)) (list r g b)))

(define modulated-texture (make-ltexture "12_color_modulation/colors.png"))

(define (make-state r g b in-progress)
  (list r g b in-progress))

(define (state-r state) (car state))

(define (state-g state) (cadr state))

(define (state-b state) (caddr state))

(define (state-in-progress? state) (cadddr state))

;; don't think i believe in this method much
;; we are really dealing with side effects
(define (make-new-state old-state)
  (define e (sdl2:poll-event!))
  (define k (if (and (sdl2:keyboard-event? e)
                     (eq? (sdl2:event-type e) 'key-down))
                (sdl2:keyboard-event-sym e)
                #f))
  (define (colour-component-adjust col adj-up adj-down)
    (+ col
       (if (and adj-up (< col 224)) 32 0)
       (if (and adj-down (> col 31)) -32 0)))
  (make-state
    (colour-component-adjust (state-r old-state) (eq? k 'q) (eq? k 'a))
    (colour-component-adjust (state-g old-state) (eq? k 'w) (eq? k 's))
    (colour-component-adjust (state-b old-state) (eq? k 'e) (eq? k 'd))
    (not (sdl2:quit-event? e))))

(define (main state)
   (begin
     (set! (sdl2:render-draw-colour screen-renderer)
           (sdl2:make-colour #xFF #xFF #xFF #xFF))
     (sdl2:render-clear! screen-renderer)
     (ltexture-set-colour! modulated-texture
                           (state-r state)
                           (state-g state)
                           (state-b state))
     (render-ltexture! modulated-texture 0 0 #f)
     (sdl2:render-present! screen-renderer)
     (when (state-in-progress? state)
       (main (make-new-state state)))))

(main (make-state #xFF #xFF #xFF #t))

