#lang racket
(module IO racket
  (require math/array (submod "rays.rkt" Pixel))
  (provide write-file read-file )
  (define (write-file pixels)
    (with-handlers ([exn:fail?
                     (lambda (v)
                       (display "write-file File operation problem")
                       (displayln (exn-message v) ))])

      (with-output-to-file  "sample.ppm"
         (
          lambda() (printf "P3~n~a ~a~n255~n" ( vector-ref   (array-shape pixels) 0) ( vector-ref   (array-shape pixels) 1 ))
          (for ((i (in-range ( vector-ref  (array-shape pixels) 0))))
            (for ((j (in-range (vector-ref  (array-shape pixels) 1 ))))
              ( printf (write-pixel (array-ref pixels  (vector i j)))))
              )
          )
          
        #:exists 'replace )
      )
    )
  
  (define (read-file) 
    (call-with-input-file "sample.ppm"
      (lambda(in) (read-string 2 in) )
      )
    )
  
  )
