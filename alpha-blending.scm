;; demo alpha blending
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

(define (ltexture-set-blend-mode! ltexture blending)
  (set! (sdl2:texture-blend-mode (car ltexture)) blending))

(define (ltexture-set-alpha! ltexture alpha)
  (set! (sdl2:texture-alpha-mod (car ltexture)) alpha))

(define modulated-texture (make-ltexture "13_alpha_blending/fadeout.png"))

(ltexture-set-blend-mode! modulated-texture 'blend)

(define background-texture (make-ltexture "13_alpha_blending/fadein.png"))

(define (make-state a in-progress)
  (list a in-progress))

(define (state-a state) (car state))

(define (state-in-progress? state) (cadr state))

(define (make-new-state old-state)
  (define e (sdl2:poll-event!))
  (define k (if (and (sdl2:keyboard-event? e)
                     (eq? (sdl2:event-type e) 'key-down))
                (sdl2:keyboard-event-sym e)
                #f))
  (define (alpha-adjust mod adj-up adj-down)
    (+ mod
       (if (and adj-up (< mod 224)) 32 0)
       (if (and adj-down (> mod 31)) -32 0)))
  (make-state
    (alpha-adjust (state-a old-state) (eq? k 'w) (eq? k 's))
    (not (sdl2:quit-event? e))))

(define (main state)
  (begin
    (set! (sdl2:render-draw-colour screen-renderer)
          (sdl2:make-colour #xFF #xFF #xFF #xFF))
    (sdl2:render-clear! screen-renderer)
    (render-ltexture! background-texture 0 0 #f)
    (ltexture-set-alpha! modulated-texture (state-a state))
    (render-ltexture! modulated-texture 0 0 #f)
    (sdl2:render-present! screen-renderer)
    (when (state-in-progress? state)
      (main (make-new-state state)))))

(main (make-state #xFF #t))

