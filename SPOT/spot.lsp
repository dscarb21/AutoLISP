;; SPOT
;; Author: David Scarborough
(setq *SPOT-VERSION* "1.2.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                                                                                       ;;
;;                                                                                SPOT COMMAND                                                                           ;;
;;                                                                                                                                                                       ;;
;;                                 Given a point (x, y), show all possible intersections with 3D PLANES, and the z values at which they occur.                           ;;
;;                                                                                                                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq faces nil)

;; MAIN COMMAND
(defun c:spot ()
  (setq initPt (getpoint "\nPick a POINT on the screen: "))
  (setq target (getTarget))
  (printHeader)
  (princ "\nPoint: (")
  (princ (strcat (rtos (car initPt)) "," (rtos (cadr initPt))))
  (princ ")\n")
  (princ "\nTarget: ")
  (princ target)  

  ;; Check if any 3D faces exist
  (initFaces)
  
  ;; Run spot command, print full reportspo 
  (printRep (spot initPt target))
  (princ)
)

;; Initialize all 3D faces
(defun initFaces ()
  (setq faces (ssget "X" '((0 . "3DFACE"))))
  (if (not faces)
    (progn
      (princ "\nNo 3D faces found. Exiting.\n")
      (exit)
    )
    (setq faceCt (sslength faces))
    
  )
)

(defun getTarget ()
  (setq target (getreal "\nTarget height: "))
)

(defun spot (point target)
  ;; Iterate through 3D faces
  (setq objList nil)
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
        (setq info (list intersection difference layerName))
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
  (princ (strcat "\n|                          Version: " *SPOT-VERSION* "                                      |"))
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
  (setq B (list (car point) (cadr point) 1000000000.00)) ; Line extends to z = 1 billion

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

(defun c:spotmp (/ filepath ret points output layerNames retList sortedPoints)
  (setq filepath (getfiled "Select CSV File" "" "csv" 4))
  (if filepath
      (progn
        
        (setq ret nil)
        (setq finals nil)
        
        (printMPHeader)
        (princ "\nInitializing faces . . .")
        (initFaces)
        (princ "\nInitializing output CSV . . .")
        (setq output (createCSV filepath))
        (princ "\nWriting headers . . .")
        (writeHeaders output)
        (princ "\nGetting input format . . .")\
        (setq input (getInputFormat))
        (princ "\nReading CSV . . .")
        (wait 2)
        (setq points (readCSV filepath))
        
        ;; Sort points by order of point number
        (defun sortPoints (points input)
          (progn
            (vl-sort (mapcar 
                      (function 
                        (lambda (pt)
                          (list pt (extractNumDesc pt input))
                        )
                      )
                      points
                    )
                    (function
                      (lambda (a b)
                        (< (atof (car (cadr a)))
                            (atof (car (cadr b))))
                      )
                    )
            )
          )
        )
        
        ;; Sort points and store numDesc
        (princ "\nSorting points . . .")
        (wait 2)
        (setq sortedPoints (sortPoints points input))
        (princ "\nPerforming SPOT . . .")
        (wait 2)
        (foreach pt sortedPoints
          (setq pointData (car pt))
          (setq numDesc (cadr pt))
          (setq xyzStrings (extractXYZStrings pointData input))
          (setq xyz (mapcar 'atof xyzStrings))
          (setq target (caddr xyz))
          (setq ret (spot (list (car xyz) (cadr xyz) 0.0) target))

          ;; Append numDesc to each element of ret to create finals
          (foreach el ret
            (setq final (append xyzStrings el))
            (setq final (append numDesc final))
            (setq finals (cons final finals))
          )
        )
        ;; Write formatted rows to CSV
        (writeData output finals)
      )
  )
  (princ)
)


(defun getInputFormat ()
  '("pNum" "pDesc" "x" "y" "z")
)

;; Extracts x y z values based off of input format previously defined
;; If input format lists "y, z, x" this function will grab the values according to the ones at those
;; indexes and return a list of those values in the order "x, y, z"

(defun extractXYZStrings (point input)
  (list (nth (find-index "x" input) point)
        (nth (find-index "y" input) point)
        (nth (find-index "z" input) point))
)

;; Extracts Number and Description bassed off of input format previously defined
;; Returns list in format: <point number, point description>
(defun extractNumDesc (point input)
  (list (nth (find-index "pNum" input) point)
    (nth (find-index "pDesc" input) point)
  )
)



(defun find-index (element body / index)
  (setq index 0)
  (while (and body (/= element (car body)))
    (setq body (cdr body))
    (setq index (1+ index))
  )
  (if body index nil)
)


;; MP header
(defun printMPHeader ()
  (princ "\n+------------------------------------------------------------------------------+")
  (princ "\n|                                                                              |")
  (princ "\n|                                S P O T                                       |")
  (princ "\n|                     M U L T I P L E     P O I N T                            |")
  (princ "\n|                                                                              |")
  (princ (strcat "\n|                          Author: David Scarborough                           |"))
  (princ (strcat "\n|                          Version: " *SPOT-VERSION* "                                      |"))
  (princ "\n|                                                                              |")
  (princ "\n+------------------------------------------------------------------------------+")
  (princ "\n")
)


;; Function to insert an element into a list in sorted order
(defun insert-sorted (element body)
  (cond
    ((null body) (list element)) ;; if list is empty, start a new list with element
    ((<= element (car body)) (cons element body)) ;; if element is less than or equal to first item, insert at beginning
    (t (cons (car body) (insert-sorted element (cdr body)))) ;; otherwise, insert element into the rest of the list
  )
)


;; Function to read and parse csv file
(defun readCSV (filepath)
  (setq ret '())
  (if (findfile filepath)
      (progn
        (setq rowNum 0)
        (setq f (open filepath "r"))
        (while (setq line (read-line f))
          (setq row (split-string line ","))          
          (if (and (/= (car row) "") (/= (cadr row) "") (/= (caddr row) "") (/= (cadddr row) "") (/= (car (cddddr row)) "")) ; Check if A, B, C, D, and E columns have non-empty content
            (progn
              (setq ret (append ret (list row))) ; add list to the list
              (setq rowNum (+ 1 rowNum))
              (if (= 0 (rem rowNum 1000)) 
                (progn 
                  (print "Reading CSV, row:")
                  (print rowNum)
                )
              )
            )
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


;; Function to create a new CSV file in the specified directory
(defun createCSV (filepath)
  ;; Extract directory path from the given filepath
  (setq directory (vl-filename-directory filepath))
  
  (setq time (substr (rtos (getvar "MILLISECS") 2 6) 1 8))

  ;; Generate a unique filename
  (setq filename (strcat "spot_output_" time ".csv"))

  ;; Construct the complete file path
  (setq newFilepath (strcat directory "\\" filename))

  ;; Attempt to open the file for writing
  (setq f (open newFilepath "w"))

  ;; Check if the file was opened successfully
  (if (not f)
    (progn
      (princ (strcat "\nError: Unable to create the file - " newFilepath))
      nil ; Return nil to indicate failure
    )
    (progn
      ;; File opened successfully, write to it and close it
      (close f) ; Close the file
      ;; Printing next step here - if I try to print before writeHeaders is called, it waits until writeHeaders is done.
      newFilepath ; Return the file path
    )
  )
)

(defun writeHeaders (filepath)
  (setq f (open filepath "w"))
  (if f
    (progn
      (write-line "Point Num.,Point Desc.,X,Y,Target Ht.,Max Ht.,Exceeds By,Layer" f)
    )
  )
  (close f)
)

(defun writeData (filepath data)
  (setq f (open filepath "a"))  ; Open the file in append mode
  (if f  ; Check if the file is successfully opened
      (progn
        (princ "\nWriting data to CSV . . .")
        (foreach cell data
          (foreach elem cell
            (princ elem f)  ; Print each element
            (princ "," f))  ; Separate each element with a space
          (princ "\n" f)
        )  ; Add a newline after each list
        (close f)
        (princ (strcat "\nComplete. Output: " filepath)) ; Notify user of file creation
      )  ; Close the file
  )
)

(defun split-string (str delimiter)
  (setq result '())
  (while (setq pos (vl-string-search delimiter str))
    (setq result (append result (list (substr str 1 pos))))
    (setq str (substr str (+ pos (strlen delimiter) 1))))
  (setq result (append result (list str)))
)

(defun orderList (mainList index)
  (sort mainList 
        (function 
          (lambda (a b) 
            (< (atof (nth index a)) (atof (nth index b)))
          )
        )
  )
)

(defun wait (seconds / stop)
  (setq stop (+ (getvar "DATE") (/ seconds 86400.0)))
  (while (> stop (getvar "DATE")))
)
