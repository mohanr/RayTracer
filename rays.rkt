#lang racket


(module Vec3d racket
  (provide vadd vminus vdiv vmult length_squared unit )
  (define ( vadd  v v1)
  (vector-map + v v1))
  (define ( vminus  v v1)
    (vector-map - v v1))
  (define ( vmult  a v1)
    (vector-map (lambda (x) (* x a)) v1 ))
  (define ( vdiv  v1 a)
    (vector-map (lambda (x)(exact->inexact (/ x a))) v1 ))
  (define (length_squared v )
    (apply + (vector->list    (vector-map  * v v))))
  (define (length v ) (sqrt( length_squared v)))
  (define ( unit v) ( vdiv v (length v)))

)

(module Pixel racket
(provide create write-pixel)
(define (create r g b)
  (bitwise-ior (arithmetic-shift (bitwise-and r #xFF) 16)
               (bitwise-ior (arithmetic-shift (bitwise-and g #xFF) 8)
                            (bitwise-and b #xFF)))
)

(define (write-pixel t)

  (format "~a ~a ~a~n" (arithmetic-shift (bitwise-and t #xFF0000) -16)
          (bitwise-and (arithmetic-shift t -8) #xFF)
          (bitwise-and t #xFF))
 
  )
)

(module Color racket 
(provide to_pixel)
(require (submod ".." Vec3d)(submod ".." Pixel))
(define (to_pixel t)
  (let* ([factor  255.99]
         [vec (vmult factor  t)]
         [r [vector-ref vec 0]]
         [g [vector-ref vec 1]]
         [b [vector-ref vec 2]])
    (create (exact-round r) (exact-round g) (exact-round b))

         )
)
)


(module Image racket
(provide create-image-array )

(require math/array)

(define (make-array rows columns)
  (array->mutable-array  ( axis-index-array (vector (inexact->exact rows) (inexact->exact columns))  0) ))

(define (create-image-array height width renderarray )
  (let
      ((pixels (make-array  height width)))
      (for ((i (range ( vector-ref  (array-shape pixels) 0))))
      (for ((j (range (vector-ref  (array-shape pixels) 1 ))))
         (array-set! pixels  (vector i j) (renderarray i j))
      ))

      pixels 
      )
      )
) 
