(in-package :sb-math)

(let ((sm (make-random-matrix (+ 3 (random 5)) :element-type 'single-float))
      
  (define-test "map-matrix-single"
    (map-matrix m1 #'square)
    (map-into m1 #'square m1))

  
