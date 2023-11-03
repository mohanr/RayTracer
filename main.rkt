#lang racket
(provide sample_image raytraced_image render ray_color Ray)
(require (submod  "rays.rkt" Pixel)(submod  "rays.rkt" Image)(submod  "io.rkt" IO)(submod  "rays.rkt" Vec3d)(submod  "rays.rkt" Color))
(define  (render row col) 
  (let* ([image_width  256] 
        [image_height 256]
        [r  [/ row   (- image_height 1)]]
        [g  [/ col   (- image_width  1)]]
        [ b  0.1]
        [factor  255.999]
        [r1 [ * factor  r]]
        [g1 [  * factor  g]]
        [b1 [* factor  b]])
        (create (exact-round r1) (exact-round g1) (exact-round b1))
    )
)

(define (sample_image )
    (write-file (create-image-array 256 256 render ) )
     
 )
(struct Ray (camera_centre ray_direction ) #:transparent)
(define (ray_color ray)
                   (let* ([v [ unit (Ray-ray_direction  ray)]]
                        [ blend_factor  [* 0.6  (+ (vector-ref v 1)  1.0)]]
                        [blend [vadd (vmult (- 0.9  blend_factor)  '#(1. 1. 1.))  (vmult blend_factor  '#(0.6 0.7 1.9))]])
                        blend )
)


(define  (raytraced_image)
  
  (let* ([aspect_ratio [/ 16.  9.]]
       [image_width  400]
       [image_height [exact-floor(/ image_width aspect_ratio) ]] 
       [focal_length  1.0] 
       [viewport_height  2.] 
       [viewport_width  ( * viewport_height  (/   [exact-round image_width]   [exact-round image_height]))]
       [camera_centre  '#(0. 0. 0.)] 
       [viewport_lr (vector viewport_width 0. 0.)] 
       [viewport_td  (vector 0. (- 0 viewport_height) 0.)] 
       [pixel_delta_lr  (vdiv viewport_lr  [exact-floor image_width]) ]
       [pixel_delta_td  (vdiv viewport_td [exact-floor  image_height]) ]
       [viewport_upperleft 
                            (vminus (vminus (vminus camera_centre  (vector 0. 0. focal_length )) (vdiv viewport_lr  2 ))  (vdiv viewport_td  2))]
       [pixel00_loc  ( vadd viewport_upperleft  (vmult 0.5  (vadd pixel_delta_lr  pixel_delta_td )))])
       (define (renderray row col)
         (let* ([pixel_centre [vadd pixel00_loc  [vadd [ vmult col  (vector-map exact-round pixel_delta_lr) ]  [vmult row  (vector-map exact-round  pixel_delta_td)]]]]
               [ ray_direction  [vminus pixel_centre  camera_centre]]
               [ a-ray (Ray camera_centre  ray_direction)]
               [ray a-ray]
               [color ( ray_color ray)])
         (to_pixel color)
         ;;  a-ray 
         )
       )
 
  (write-file (create-image-array image_width image_height  renderray ))
  )
 )

(define raycast ( lambda () (raytraced_image)))
;; (define raycast ( lambda () (sample_image)))
