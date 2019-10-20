;; demo mouse events
(import (prefix sdl2 "sdl2:")
        (prefix sdl2-image "img:")
        (chicken condition))

(define screen-width 640)
(define screen-height 480)
(define button-width 300)
(define button-height 200)
(define button-sprite-mouse-out 0)
(define button-sprite-mouse-over-motion 1)
(define button-sprite-mouse-down 2)
(define button-sprite-mouse-up 3)

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

(define button-sprite-sheet-texture
  (make-ltexture "17_mouse_events/button.png"))

(define sprite-clips
  (list->vector
    (list (sdl2:make-rect 0 0 button-width button-height)
          (sdl2:make-rect 0 200 button-width button-height)
          (sdl2:make-rect 0 400 button-width button-height)
          (sdl2:make-rect 0 600 button-width button-height))))

(define (make-lbutton x y sprite)
  (list x y sprite))

(define (lbutton-x lbutton) (car lbutton))

(define (lbutton-y lbutton) (cadr lbutton))

(define (lbutton-sprite lbutton) (caddr lbutton))

(define (handle-lbutton-event lbutton event)
  (define (inside? x y)
    (and (>= x (lbutton-x lbutton))
         (<= x (+ (lbutton-x lbutton) button-width))
         (>= y (lbutton-y lbutton))
         (<= y (+ (lbutton-y lbutton) button-height))))
  (define (event-inside? event)
     (if (eq? (sdl2:event-type event) 'mouse-motion)
         (inside? (sdl2:mouse-motion-event-x event)
                  (sdl2:mouse-motion-event-y event))
         (inside? (sdl2:mouse-button-event-x event)
                  (sdl2:mouse-button-event-y event))))
  (make-lbutton
    (lbutton-x lbutton)
    (lbutton-y lbutton)
    (if (event-inside? event)
        (case (sdl2:event-type event)
          ((mouse-motion) button-sprite-mouse-over-motion)
          ((mouse-button-down) button-sprite-mouse-down)
          ((mouse-button-up) button-sprite-mouse-up))
        button-sprite-mouse-out)))

(define (render-lbutton! lbutton)
  (render-ltexture! button-sprite-sheet-texture
                    (lbutton-x lbutton)
                    (lbutton-y lbutton)
                    (vector-ref sprite-clips (lbutton-sprite lbutton))))

(define (mouse-event? event)
  (and (sdl2:event? event)
       ;; not exhaustive, just the ones we're interested in
       (memq (sdl2:event-type event)
             '(mouse-motion mouse-button-down mouse-button-up))))
  
(define (make-state lbuttons in-progress) (list lbuttons in-progress))

(define (state-buttons state) (car state))

(define (state-in-progress? state) (cadr state))

(define (make-new-state old-state)
  (let ((event (sdl2:poll-event!)))
    (make-state
      (if (mouse-event? event)
          (map (lambda (btn) (handle-lbutton-event btn event)) (car old-state))
          (car old-state))
      (not (sdl2:quit-event? event)))))

(define (main state)
  (when (state-in-progress? state)
    (begin
      (set! (sdl2:render-draw-colour screen-renderer)
            (sdl2:make-colour #xFF #xFF #xFF #xFF))
      (sdl2:render-clear! screen-renderer)
      (for-each render-lbutton! (state-buttons state))
      (sdl2:render-present! screen-renderer)
      (main (make-new-state state)))))

(main
  (make-state
    (list (make-lbutton 0 0 button-sprite-mouse-out)
          (make-lbutton (- screen-width button-width) 0 button-sprite-mouse-out)
          (make-lbutton 0 (- screen-height button-height) button-sprite-mouse-out)
          (make-lbutton (- screen-width button-width)
                        (- screen-height button-height)
                        button-sprite-mouse-out))
    #t))

