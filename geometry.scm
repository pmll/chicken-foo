;; demo rectangle line and poinr drawing
(import (prefix sdl2 "sdl2:")
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

(sdl2:set-hint! 'render-scale-quality "1")

(define window
  (sdl2:create-window! "SDL Tutorial" 0 0 screen-width screen-height))

(define screen-renderer (sdl2:create-renderer! window -1 '(accelerated)))

(define white (sdl2:make-colour #xFF #xFF #xFF #xFF))
(define red (sdl2:make-colour #xFF #x00 #x00 #x00))
(define green (sdl2:make-colour #x00 #xFF #x00 #xFF))
(define blue (sdl2:make-colour #x00 #x00 #xFF #xFF))
(define yellow (sdl2:make-colour #xFF #xFF #x00 #x00))

(define (while-not-quit proc)
  (when (not (sdl2:quit-requested?))
    (begin
      (proc)
      (while-not-quit proc))))

(while-not-quit
  (lambda ()
    (begin
      (set! (sdl2:render-draw-colour screen-renderer) white)
      (sdl2:render-clear! screen-renderer)
      (set! (sdl2:render-draw-colour screen-renderer) red)
      (sdl2:render-fill-rect! screen-renderer
                              (sdl2:make-rect (quotient screen-width 4)
                                              (quotient screen-height 4)
                                              (quotient screen-width 2)
                                              (quotient screen-height 2)))
      (set! (sdl2:render-draw-colour screen-renderer) green)
      (sdl2:render-draw-rect! screen-renderer
                              (sdl2:make-rect (quotient screen-width 6)
                                              (quotient screen-height 6)
                                              (quotient (* screen-width 2) 3)
                                              (quotient (* screen-height 2) 3)))
      (set! (sdl2:render-draw-colour screen-renderer) blue)
      (sdl2:render-draw-line! screen-renderer
                              0
                              (quotient screen-height 2)
                              screen-width
                              (quotient screen-height 2))
      (set! (sdl2:render-draw-colour screen-renderer) yellow)
      (let loop ((i 0))
        (when (< i screen-height)
          (begin
            (sdl2:render-draw-point! screen-renderer
                                     (quotient screen-width 2) i)
            (loop (+ i 4)))))
      (sdl2:render-present! screen-renderer))))

