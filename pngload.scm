;; demo using extended image library to load png
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

(define screen-surface (sdl2:window-surface window))

(define png-surface
  (let ((file-surface (img:load "06_extension_libraries_and_loading_other_image_formats/loaded.png")))
  (sdl2:convert-surface file-surface (sdl2:surface-format screen-surface))))
  
(define (while-not-quit proc)
  (when (not (sdl2:quit-requested?))  ;; this is handy, no need to take event off queue checking for quit
    (begin
      (proc)
      (while-not-quit proc))))

(while-not-quit
  (lambda ()
    (begin
      (sdl2:blit-surface! png-surface #f screen-surface #f)
      (sdl2:update-window-surface! window))))

