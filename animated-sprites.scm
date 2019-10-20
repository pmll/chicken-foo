;; demo animated sprites and vsync
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

(define screen-renderer
  (sdl2:create-renderer! window -1 '(accelerated present-vsync)))

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

(define sprite-sheet-texture
  (make-ltexture "14_animated_sprites_and_vsync/foo.png"))

(define sprite-clips
  (list->vector (list (sdl2:make-rect 0 0 64 205)
                      (sdl2:make-rect 64 0 64 205)
                      (sdl2:make-rect 128 0 64 205)
                      (sdl2:make-rect 196 0 64 205))))

(define (make-state frame in-progress)
  (list frame in-progress))

(define (state-frame state) (car state))

(define (state-in-progress? state) (cadr state))

(define (make-new-state old-state)
  (define e (sdl2:poll-event!))
  (make-state
    (remainder (+ (state-frame old-state) 1) 4)
    (not (sdl2:quit-event? e))))

(define (main state)
   (begin
     (set! (sdl2:render-draw-colour screen-renderer)
           (sdl2:make-colour #xFF #xFF #xFF #xFF))
     (sdl2:render-clear! screen-renderer)
     (render-ltexture! sprite-sheet-texture
                       (quotient (- screen-width 64) 2)
                       (quotient (- screen-height 205) 2)
                       (vector-ref sprite-clips (state-frame state)))
     (sdl2:render-present! screen-renderer)
     (when (state-in-progress? state)
       (main (make-new-state state)))))

(main (make-state 0 #t))

