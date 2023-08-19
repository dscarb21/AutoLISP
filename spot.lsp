;; SPOT
;; Author: David Scarborough
(setq *SPOT-VERSION* "1.0.01")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                                                                                       ;;
;;                                                                                SPOT COMMAND                                                                           ;;
;;                                                                                                                                                                       ;;
;;                                 Given a point (x, y), show all possible intersections with 3D PLANES, and the z values at which they occur.                           ;;
;;                                                                                                                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq target nil)
(setq objList nil)
(setq faces nil)

;; MAIN COMMAND
(defun c:spot ()
  (setq initPt (getpoint "\nPick a POINT on the screen: "))
  (setq target (getreal "\nTarget height: "))
  (princ "\nPoint: (")
  (princ (strcat (rtos (car initPt)) "," (rtos (cadr initPt))))
  (princ ")\n")
  (princ "\nTarget: ")
  (princ target)  

  ;; Check if any 3D faces exist
  (setq faces (ssget "X" '((0 . "3DFACE"))))
  (if (not faces)
    (progn
      (princ "\nNo 3D faces found. Exiting.\n")
      (exit)
    )
    (setq faceCt (sslength faces))
    
  )
  
  (printHeader)
  ;; Run spot command, print full report
  (printRep (spot initPt))
  (princ)
)

(defun spot (point)
  ;; Iterate through 3D faces
  (setq i 0)
  (repeat faceCt
    (setq curPl (ssname faces i))
    (setq data (entget curPl)) ;; Get the entity data of the plane
    (setq layerName (cdr (assoc 8 data)))
    (if (raycast point curPl i) 
      (progn        
        ;; Check intersection
        (setq intersection (calc_inter point curPl))
        ;; Calculate difference
        (setq difference (- target intersection))
        ;; Create list (Z Intersection, difference, Layer) - difference will only be added if > 0
        (setq info (if (<= difference 0) (list intersection nil layerName) (list intersection difference layerName)))
        ;; Add to list of lists
        (setq objList (append objList (list info)))
      )
    )
    (setq i (1+ i))
  )
  
  objList
  
)

;; Name and Version
(defun printHeader ()
  (princ "\n+------------------------------------------------------------------------------+")
  (princ "\n|                                                                              |")
  (princ "\n|                                 S P O T                                      |")
  (princ "\n|                                                                              |")
  (princ (strcat "\n|                          Author: David Scarborough                           |"))
  (princ (strcat "\n|                          Version: " *SPOT-VERSION* "                                     |"))
  (princ "\n|                                                                              |")
  (princ "\n+------------------------------------------------------------------------------+")
  (princ "\n")
)

;; Print FULL Report
(defun printRep (objList)
  ;; Sort list from greatest to least
  (setq objList (vl-sort objList 'compare))

  (setq tableStr "") ;; Initialize the table string

  ;; Add the headers to the table string
  (setq tableStr (strcat tableStr "\nZ Intersection  | Exceeds By      | Layer\n"))
  (setq tableStr (strcat tableStr "-------------------------------------------------\n"))

  ;; Iterate over each tuple in the list
  (foreach triplet objList
    ;; Extract the z intersection, difference and layer name from the triplet
    (setq intersection (rtos (car triplet)))
    (setq difference (if (cadr triplet) (rtos (cadr triplet)) "")) ; If difference is nil (<= 0), replace it with an empty string
    (setq layerName (caddr triplet))

    ;; Pad intersection and difference with spaces to ensure columns align
    (repeat (- 15 (strlen intersection))
      (setq intersection (strcat intersection " "))
    )
    (repeat (- 15 (strlen difference))
      (setq difference (strcat difference " "))
    )

    ;; Add the row to the table string
    (setq tableStr (strcat tableStr intersection " | " difference " | " layerName "\n")) ; Now prints intersection, difference (if > 0), then layer
  )

  ;; Print the table
  (princ tableStr)
)

;; Compare the first elements, which are the intersections
(defun compare (t1 t2)
  (> (car t1) (car t2)) 
)

;;     RAYCAST
;; Determines if a 2D projection of a given point intersects with a 3D triangular plane.
;; The function checks if the 2D projection of the point lies within the triangle formed 
;; by the three vertices of the 3D plane.
;; 
;; Parameters:
;;   - point: The point to be projected.
;;   - plane: The 3D plane (triangle) to check for intersection.
;;   - i: An index or identifier for the plane (not directly used in the intersection logic.
;; 
;; Returns:
;;   - True if the 2D projection of the point intersects with the 3D plane.
;;   - False otherwise.
;;
(defun raycast (point plane i)
  ;; Get the entity data of the plane
  (setq el (entget plane))

  ;; Get the coordinates of the three points
  (setq p1 (cdr (assoc 10 el))) ; first point
  (setq p2 (cdr (assoc 11 el))) ; second point
  (setq p3 (cdr (assoc 12 el))) ; third point

  ;; Ignore Z coordinates for this 2D algorithm
  (setq x (car point))
  (setq y (cadr point))
  (setq p1 (list (car p1) (cadr p1)))
  (setq p2 (list (car p2) (cadr p2)))
  (setq p3 (list (car p3) (cadr p3)))

  ;; Initialize within flag
  (setq within nil)

  ;; Check each edge of the triangle
  (foreach edge (list (list p1 p2) (list p2 p3) (list p3 p1))
    (setq ep1 (car edge))
    (setq ep2 (cadr edge))
    ;; Raycast algo
    (if (and (/= (>= (cadr ep1) y) (>= (cadr ep2) y)) (<= x (+ (/ (* (- (car ep2) (car ep1)) (- y (cadr ep1))) (- (cadr ep2) (cadr ep1))) (car ep1))))
      (setq within (not within))
    )
  )

  ;; Return the result
  within
)

(defun calc_inter (point plane)
  ;; Define line parameters
  (setq A (list (car point) (cadr point) (caddr point)))
  (setq B (list (car point) (cadr point) 100.0)) ; Line extends to z = 100

  ;; Get the entity data of the plane
  (setq el (entget plane))

  ;; Define plane parameters
  (setq A_p (cdr (assoc 10 el))) ; first point
  (setq B_p (cdr (assoc 11 el))) ; second point
  (setq C_p (cdr (assoc 12 el))) ; third point
  (setq D A_p)
  (setq N (cross-product (mapcar '- B_p A_p) (mapcar '- C_p A_p))) ; Normal of the plane

  ;; Calculate intersection
  (setq tval (/ (dot-product (mapcar '- D A) N) (dot-product (mapcar '- B A) N)))
  (setq intersection (mapcar '+ A (scale-vector tval (mapcar '- B A))))

  ;; Return z coordinate of intersection
  (caddr intersection)
)

(defun dot-product (v1 v2)
  (+ (* (car v1) (car v2)) (* (cadr v1) (cadr v2)) (* (caddr v1) (caddr v2)))
)

(defun cross-product (v1 v2)
  (list (- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2)))
        (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2)))
        (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))
  )
)

(defun scale-vector (scalar vector)
  (mapcar (function (lambda (x) (* x scalar))) vector))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                                                                                       ;;
;;                                                              SPOT (MULTIPLE POINTS) COMMAND                                                                           ;;
;;                                                                                                                                                                       ;;
;;                                                                                                                                                                       ;;
;;                                                                                                                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:spotmp (/ filepath)
  (setq filepath (getfiled "Select CSV File" "" "csv" 4))
  (if filepath
      (progn
        (setq points (readCSV filepath))
        ;; Process csvData as needed
        (princ "\nPoints: ")
        (princ points)
      )
  )
  (princ)
)

;; Function to read and parse a CSV file, then print the 'A' column
(defun readCSV (filepath)
  (setq ret '())
  (if (findfile filepath)
      (progn
        (setq f (open filepath "r"))
        
        ;; Iterate through all lines
        (while (setq line (read-line f))
          (setq row (split-string line ","))
          (if (and (/= (car row) "") (/= (cadr row) "")) ; Check if both A and B columns have non-empty content
              (setq ret (append ret (list (cons (car row) (cadr row))))) ; Add tuple to the list
          )
        )
        (close f)
      )
      (progn
        (princ "\nError: File not found.")
      )
  )
  ret
)

(defun split-string (str delimiter)
  (setq result '())
  (while (setq pos (vl-string-search delimiter str))
    (setq result (append result (list (substr str 1 pos))))
    (setq str (substr str (+ pos (strlen delimiter) 1))))
  (setq result (append result (list str)))
)
