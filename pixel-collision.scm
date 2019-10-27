;; disappointingly, there is not some hardware accelerated pixel collision
;; detection - it just involves breaking down your image into smaller rectangles
(import (prefix sdl2 "sdl2:")
        (prefix sdl2-image "img:")
        (chicken condition)
        (list-utils))

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

(define dot-vel 1)

(define dot-texture (make-ltexture "28_per-pixel_collision_detection/dot.bmp"))

(define (make-dot x y vel-x vel-y) (list x y vel-x vel-y))

(define (dot-x dot) (car dot))

(define (dot-y dot) (cadr dot))

(define (dot-vel-x dot) (caddr dot))

(define (dot-vel-y dot) (cadddr dot))

(define dot-rects
  '((6 1) (10 1) (14 1) (16 2) (18 2) (20 6) (18 2) (16 2) (14 1) (10 1) (6 1)))

(define (dot-colliders dot)
  (define (make-colliders dot-rects r)
    (if (null? dot-rects)
        '()
        (let ((wh (car dot-rects)))
          (cons
            (sdl2:make-rect (+ (dot-x dot) (quotient (- dot-width (car wh)) 2))
                            (+ (dot-y dot) r)
                            (car wh)
                            (cadr wh))
            (make-colliders (cdr dot-rects) (+ r (cadr wh)))))))
  (make-colliders dot-rects 0))

(define (collision? colliders-a colliders-b)
  (define (collision-in-list? collider colliders)
    (ormap (lambda (other-collider)
             (sdl2:has-intersection? collider other-collider))
           colliders))
  (ormap (lambda (collider)
           (collision-in-list? collider colliders-b))
          colliders-a))
  
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

(define (move-dot dot other-colliders)
  ;; by considering x first: when heading for a diagonal intersection, x axis
  ;; wins out. when diagonally avoiding an intersection, x axis loses out.
  (let* ((pot-x (+ (dot-x dot) (dot-vel-x dot)))
         (x (if (or (< pot-x 0)
                    (>= pot-x (- screen-width dot-width))
                    (collision? (dot-colliders (make-dot pot-x (dot-y dot) 0 0))
                                other-colliders))
                (dot-x dot)
                pot-x))
         (pot-y (+ (dot-y dot) (dot-vel-y dot)))
         (y (if (or (< pot-y 0)
                    (>= pot-y (- screen-height dot-height))
                    (collision? (dot-colliders (make-dot x pot-y 0 0))
                                other-colliders))
                (dot-y dot)
                pot-y)))
    (make-dot x y (dot-vel-x dot) (dot-vel-y dot))))

(define (render-dot! dot)
  (render-ltexture! dot-texture (dot-x dot) (dot-y dot) #f))

(define other-dot
  (make-dot (quotient screen-width 4) (quotient screen-height 4) 0 0))

(define (main old-dot)
  (when (not (sdl2:quit-requested?))
    (let ((dot (move-dot (handle-dot-events old-dot (sdl2:get-events! 99))
                         (dot-colliders other-dot))))
      (set! (sdl2:render-draw-colour screen-renderer)
            (sdl2:make-colour #xFF #xFF #xFF #xFF))
      (sdl2:render-clear! screen-renderer)
      (render-dot! dot)
      (render-dot! other-dot)
      (sdl2:render-present! screen-renderer)
      (main dot))))

(main (make-dot 0 0 0 0 ))

