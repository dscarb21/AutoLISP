(defun pseudo-random (seed)
  (setq a 1664525)
  (setq c 1013904223)
  (setq m (expt 2 32))
  (setq seed (rem (+ (* a seed) c) m))
  seed
)

(defun create-3d-faces ()
  (setq faces '())
  (setq seed 0) ; Initialize seed
  (repeat 20
    (setq seed (pseudo-random seed)) ; Update seed for next random-like number
    (setq x (+ 15 (rem seed 16)))
    (setq seed (pseudo-random seed)) ; Update seed for next random-like number
    (setq y (+ 10 (rem seed 11)))
    (setq seed (pseudo-random seed)) ; Update seed for next random-like number
    (if (= (rem seed 101) 0) ; Prevent division by zero
        (setq z 1)
        (setq z (rem seed 101)))
    (setq pt1 (vlax-3d-point (list x y z)))
    (setq seed (pseudo-random seed)) ; Update seed for next random-like number
    (setq pt2 (vlax-3d-point (list (+ x 1) (+ y 1) z)))
    (setq seed (pseudo-random seed)) ; Update seed for next random-like number
    (setq pt3 (vlax-3d-point (list (+ x 1) y z)))
    (setq face (list pt1 pt2 pt3))
    (setq faces (cons face faces))
  )
  (setq faces (reverse faces))
  (command "_.3dface" faces)
)

(create-3d-faces)
