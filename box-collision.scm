;; box collision
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

(define screen-renderer (sdl2:create-renderer! window -1 '(accelerated present-vsync)))

(define (make-ltexture file)
  (let ((file-surface (img:load file)))
    (set! (sdl2:surface-colour-key file-surface)
          (sdl2:map-rgb (sdl2:surface-format file-surface) #x0 #xFF #xFF))
    (list (sdl2:create-texture-from-surface screen-renderer file-surface)
          (sdl2:surface-w file-surface)
          (sdl2:surface-h file-surface))))

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

(define (key-press-event? event event-type key)
  (and (sdl2:keyboard-event? event)
       (eq? (sdl2:event-type event) event-type)
       (eq? (sdl2:keyboard-event-repeat event) 0)
       (eq? (sdl2:keyboard-event-sym event) key)))

(define (key-down? event key) (key-press-event? event 'key-down key))

(define (key-up? event key) (key-press-event? event 'key-up key))

(define dot-width 20)

(define dot-height 20)

(define dot-vel 10)

(define dot-texture (make-ltexture "27_collision_detection/dot.bmp"))

(define (make-dot x y vel-x vel-y) (list x y vel-x vel-y))

(define (dot-x dot) (car dot))

(define (dot-y dot) (cadr dot))

(define (dot-vel-x dot) (caddr dot))

(define (dot-vel-y dot) (cadddr dot))

(define (handle-dot-events dot events)
  (if (null? events)
      dot
      (handle-dot-events
        (make-dot
          (dot-x dot)
          (dot-y dot)
          (+ (dot-vel-x dot) (if (key-down? (car events) 'right) dot-vel 0)
                             (if (key-down? (car events) 'left) (- dot-vel) 0)
                             (if (key-up? (car events) 'right) (- dot-vel) 0)
                             (if (key-up? (car events) 'left) dot-vel 0))
          (+ (dot-vel-y dot) (if (key-down? (car events) 'down) dot-vel 0)
                             (if (key-down? (car events) 'up) (- dot-vel) 0)
                             (if (key-up? (car events) 'down) (- dot-vel) 0)
                             (if (key-up? (car events) 'up) dot-vel 0)))
          (cdr events))))

(define (move-dot dot wall)
  ;; by considering x first: when heading for a diagonal intersection, x axis
  ;; wins out. when diagonally avoiding an intersection, x axis loses out.
  (let* ((pot-x (+ (dot-x dot) (dot-vel-x dot)))
         (x (if (or (< pot-x 0)
                    (>= pot-x (- screen-width dot-width))
                    (sdl2:has-intersection?
                      (sdl2:make-rect pot-x (dot-y dot) dot-width dot-height)
                      wall))
                (dot-x dot)
                pot-x))
         (pot-y (+ (dot-y dot) (dot-vel-y dot)))
         (y (if (or (< pot-y 0)
                    (>= pot-y (- screen-height dot-height))
                    (sdl2:has-intersection?
                      (sdl2:make-rect x pot-y dot-width dot-height)
                      wall))
                (dot-y dot)
                pot-y)))
    (make-dot x y (dot-vel-x dot) (dot-vel-y dot))))

(define (render-dot! dot)
  (render-ltexture! dot-texture (dot-x dot) (dot-y dot) #f))

(define wall (sdl2:make-rect 300 40 40 400))

(define (main old-dot)
  (when (not (sdl2:quit-requested?))
    (let ((dot (move-dot (handle-dot-events old-dot (sdl2:get-events! 99)) wall)))
      (set! (sdl2:render-draw-colour screen-renderer)
            (sdl2:make-colour #xFF #xFF #xFF #xFF))
      (sdl2:render-clear! screen-renderer)
      (set! (sdl2:render-draw-colour screen-renderer)
            (sdl2:make-colour 0 0 0 #xFF))
      (sdl2:render-draw-rect! screen-renderer wall)
      (render-dot! dot)
      (sdl2:render-present! screen-renderer)
      (main dot))))

(main (make-dot 0 0 0 0 ))

