;; demo capturing the quit event
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

(define window
  (sdl2:create-window! "SDL Tutorial" 0 0 screen-width screen-height))

(define screen-surface (sdl2:window-surface window))

(define x-out
  (sdl2:load-bmp "03_event_driven_programming/x.bmp"))

(define (while-not-quit proc)
  (when (not (sdl2:quit-event? (sdl2:poll-event!)))
    (begin
      (proc)
      (while-not-quit proc))))

(while-not-quit
  (lambda ()
    (begin
      (sdl2:blit-surface! x-out #f screen-surface #f)
      (sdl2:update-window-surface! window))))

