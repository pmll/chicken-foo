;; demo loading an image to a surface and displaying it to the window surface
;; apparently this is using software to display the image and is not the most
;; efficiemt
(import (prefix sdl2 "sdl2:")
        (chicken condition))

(define screen-width 640)
(define screen-height 480)

;; Initialise SDL
(sdl2:set-main-ready!)
(sdl2:init! '(video))

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

(define hello-world
  (sdl2:load-bmp "02_getting_an_image_on_the_screen/hello_world.bmp"))

(sdl2:blit-surface! hello-world #f screen-surface #f)

(sdl2:update-window-surface! window)

(sdl2:delay! 2000)

