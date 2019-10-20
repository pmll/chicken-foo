;; demo keyboard events
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

(define default-surface (sdl2:load-bmp "04_key_presses/press.bmp"))
(define up-surface (sdl2:load-bmp "04_key_presses/up.bmp"))
(define down-surface (sdl2:load-bmp "04_key_presses/down.bmp"))
(define left-surface (sdl2:load-bmp "04_key_presses/left.bmp"))
(define right-surface (sdl2:load-bmp "04_key_presses/right.bmp"))

(define (make-state surface in-progress)
  (list surface in-progress))

(define (state-surface state)
  (car state))

(define (state-in-progress? state)
  (cadr state))

(define (make-new-state old-state)
  (define e (sdl2:poll-event!))
  (make-state
    ;; demoinstrate identifying a keyboard event and getting the key symbol
    (if (and (sdl2:keyboard-event? e)
             (eq? (sdl2:event-type e) 'key-down))
        (case (sdl2:keyboard-event-sym e)
          ((up) up-surface)
          ((down) down-surface)
          ((left) left-surface)
          ((right) right-surface) 
          (else default-surface))
         (state-surface old-state))
    (not (sdl2:quit-event? e))))

(define (main state)
  (begin
     (sdl2:blit-surface! (state-surface state) #f screen-surface #f)
     (sdl2:update-window-surface! window)
     (when (state-in-progress? state)
        (main (make-new-state state)))))

(main (make-state default-surface #t))

