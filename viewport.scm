;; demo viewports
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

(define my-texture
  (let ((file-surface (img:load "09_the_viewport/viewport.png")))
    (sdl2:create-texture-from-surface screen-renderer file-surface)))

(define top-left-viewport (sdl2:make-rect 0
                                          0
                                          (quotient screen-width 2)
                                          (quotient screen-height 2)))

(define top-right-viewport (sdl2:make-rect (quotient screen-width 2)
                                           0
                                           (quotient screen-width 2)
                                           (quotient screen-height 2)))


(define bottom-viewport (sdl2:make-rect 0
                                        (quotient screen-height 2)
                                        screen-width
                                        (quotient screen-height 2)))

(define (while-not-quit proc)
  (when (not (sdl2:quit-requested?))
    (begin
      (proc)
      (while-not-quit proc))))

(set! (sdl2:render-draw-colour screen-renderer) (sdl2:make-colour #xFF #xFF #xFF #xFF))

(while-not-quit
  (lambda ()
    (begin
      (sdl2:render-clear! screen-renderer)
      (set! (sdl2:render-viewport screen-renderer) top-left-viewport)
      (sdl2:render-copy! screen-renderer my-texture)
      (set! (sdl2:render-viewport screen-renderer) top-right-viewport)
      (sdl2:render-copy! screen-renderer my-texture)
      (set! (sdl2:render-viewport screen-renderer) bottom-viewport)
      (sdl2:render-copy! screen-renderer my-texture)
      (sdl2:render-present! screen-renderer))))

