;; timer demo
(import (prefix sdl2 "sdl2:")
        (prefix sdl2-image "img:")
        (prefix sdl2-ttf "ttf:")
        (chicken condition)
        (format))

(define screen-width 640)
(define screen-height 480)

;; Initialise SDL
(sdl2:set-main-ready!)
(sdl2:init! '(video) '(events))
(ttf:init!)

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

(define font (ttf:open-font "22_timing/lazy.ttf" 28))

(define (make-ltexture file)
  (let ((file-surface (img:load file)))
    (set! (sdl2:surface-colour-key file-surface)
          (sdl2:map-rgb (sdl2:surface-format file-surface) #x0 #xFF #xFF))
    (list (sdl2:create-texture-from-surface screen-renderer file-surface)
          (sdl2:surface-w file-surface)
          (sdl2:surface-h file-surface))))

(define (make-ltexture-from-text text colour)
  (let ((text-surface (ttf:render-text-solid font text colour)))
    (list (sdl2:create-texture-from-surface screen-renderer text-surface)
          (sdl2:surface-w text-surface) 
          (sdl2:surface-h text-surface))))

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

(define prompt-text-texture
  (make-ltexture-from-text "Press Enter to Reset Start Time."
                           (sdl2:make-colour 0 0 0)))

(define (make-state start-time in-progress) (list start-time in-progress))

(define (state-start-time state) (car state))

(define (state-in-progress? state) (cadr state))

(define (render-elapsed-time! since)
  (let ((texture
          (make-ltexture-from-text
            (format "Milliseconds since start time ~D"
                    (- (sdl2:get-ticks) since))
            (sdl2:make-colour 0 0 0 ))))
  (render-ltexture! texture
                    (quotient (- screen-width (ltexture-w texture)) 2)
                    (quotient (- screen-height (ltexture-h texture)) 2)
                    #f)))

(define (process-state old-state)
  (let ((e (sdl2:poll-event!)))
    (make-state
      (if (and (sdl2:keyboard-event? e)
               (eq? (sdl2:event-type e) 'key-down)
               (eq? (sdl2:keyboard-event-sym e) 'return))
          (sdl2:get-ticks)
          (state-start-time old-state))
      (not (sdl2:quit-event? e)))))
  

(define (main state)
  (when (state-in-progress? state)
    (begin
      (set! (sdl2:render-draw-colour screen-renderer)
          (sdl2:make-colour #xFF #xFF #xFF #xFF))
      (sdl2:render-clear! screen-renderer)
      (render-ltexture! prompt-text-texture 
                        (quotient (- screen-width
                                     (ltexture-w prompt-text-texture))
                                  2)
                        0
                        #f)
      (render-elapsed-time! (state-start-time state))
      (sdl2:render-present! screen-renderer)
      (main (process-state state)))))

(main (make-state 0 #t))

