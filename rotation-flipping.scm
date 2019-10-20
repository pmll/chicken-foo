;; demo rotation and flipping
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

(define (ltexture-w ltexture) (cadr ltexture))

(define (ltexture-h ltexture) (caddr ltexture))

(define (render-ltexture! ltexture x y clip angle centre flip)
  (sdl2:render-copy-ex!
    screen-renderer
    (car ltexture)
    clip
    (sdl2:make-rect x
                    y
                    (if clip (sdl2:rect-w clip) (cadr ltexture))
                    (if clip (sdl2:rect-h clip) (caddr ltexture)))
    angle
    centre
    flip))

(define arrow-texture (make-ltexture "15_rotation_and_flipping/arrow.png"))

(define (make-state degrees flip-type in-progress)
  (list degrees flip-type in-progress))

(define (state-degrees state) (car state))

(define (state-flip-type state) (cadr state))

(define (state-in-progress state) (caddr state))

;; don't think i believe in this method much
;; we are really dealing with side effects
(define (make-new-state old-state)
  (define e (sdl2:poll-event!))
  (define k (if (and (sdl2:keyboard-event? e)
                     (eq? (sdl2:event-type e) 'key-down))
                (sdl2:keyboard-event-sym e)
                #f))
  (make-state
    (+ (state-degrees old-state) (if (eq? k 'a) -60 0) (if (eq? k 'd) 60 0))
    (cond ((eq? k 'q) '(horizontal))
          ((eq? k 'w) '(vertical))
          ((eq? k 'e) '())
          (else (state-flip-type old-state)))
    (not (sdl2:quit-event? e))))

(define (main state)
   (begin
     (set! (sdl2:render-draw-colour screen-renderer)
           (sdl2:make-colour #xFF #xFF #xFF #xFF))
     (sdl2:render-clear! screen-renderer)
     (render-ltexture! arrow-texture 
                       (quotient (- screen-width (ltexture-w arrow-texture)) 2)
                       (quotient (- screen-height (ltexture-h arrow-texture)) 2)
                       #f
                       (state-degrees state)
                       #f
                       (state-flip-type state))
     (sdl2:render-present! screen-renderer)
     (when (state-in-progress state)
       (main (make-new-state state)))))

(main (make-state 0 '() #t))

