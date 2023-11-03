#lang racket
(require math/array)

(require rackunit  "main.rkt"  (submod  "rays.rkt" Vec3d )
                               (submod  "io.rkt" IO)
                               (submod  "rays.rkt" Pixel)
                               (submod  "rays.rkt" Image )
                               (submod  "rays.rkt" Color))
  (check-equal? (vadd '#(3 2 3)  '#(0 1 2)) '#(3 3 5))
  (check-equal? (vminus '#(2 2 3)  '#(0 1 2)) '#(2 1 1))
  (check-equal? (vmult  3  '#(0 1 2)) '#(0 3 6) "vmult failure")
  (check-equal? (length_squared  '#(0 1 2)) 5  "length-squared failure")
  (check-equal? (write-pixel 1) "0 0 1\n")
  (check-equal? (create 1 2 3) 66051 "Create Pixels")
  (check-equal? (render 1 1) 65818)
  ;; (check-equal? ( ray_color (Ray '#(1 1 1) '#(2 2 2))) '#(0.5214359353944898 0.6160769515458674 1.751769145362398)
  (print ( ray_color (Ray '#(1 1 1) '#(2 3 4)) ));; 0.3713910.5570860.7427818822461 
  (print (unit '#(1 1 1)))
  (printf "Image height is ~a~n" (exact-floor (/ 400 (/ 16. 9.))))
  
  (define (pretty-print array)
    (for ((i (in-range ( vector-ref  (array-shape array) 0))))
    (for ((j (in-range (vector-ref  (array-shape array) 1 ))))
        (printf "~a\t" (array-ref array (vector i j) )))
      (newline)))

 ;; (pretty-print (create-image-array 5 5 render))
  (sample_image)
;; (raytraced_image)

