(in-package :cl-mst)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code from library OpenMusic/PWGL fv-morphologie-20111205
;; source: http://www.fredvoisin.com/spip.php?article113
;;;;;;;;;; Prim's algorithm.

(defgeneric a-zero (x &optional quasi-zero)
 (:documentation "Replace all zeros by small value (optional: quasi-zero value).
If x is a graph or tree (a list of lists), change only the third element of each node considered to be a distance."))
;; a-zero is [...] a kind of deleuzian differentiation...

(defmethod a-zero ((x number) &optional (quasi-zero .001))
  (if (zerop x) (random quasi-zero) x))

(defmethod a-zero ((x list) &optional (quasi-zero .001))  ;; not to be removed used for tres etc
  (if (not (member nil (mapcar #'listp x)))
      (mapcar #'(lambda (a) (append (butlast a) (a-zero (last a) quasi-zero))) x)
      (mapcar #'(lambda (a) (a-zero a quasi-zero)) x)))

(defun add-sommet (arete set)
  (if (not (member (car arete) set))
      (push (car arete) set)
    (when (not (member (cadr arete) set)) (push (cadr arete) set)))
  (values set))

(defun lookformin-if-set (i dist set)
  "Returns a list with i and its nearest point and the distance separating them,
given the semi-matrix of distances dist."
  (let ((d-min (car (sort (mapcar #'caddr dist) '>)))
        arete)
    (dolist (n dist)
        (when (member i (butlast n))
          (when (and (< (caddr n) d-min)
                     (not (member (car (remove i (butlast n))) set)))
            (setf d-min (caddr n)
                  arete n))))
    arete))

(defun remove-arete (arete set)
  (remove-if #'(lambda (x) (equalp arete x)) set))

(defun zeroize (x e)
  (if (> (third x) e) x (list (car x) (cadr x) 0)))

(defun prim (dist-mat)
  "Algorithm from: E. Diday & all, 1982 : Elements d'analyse de donnees, Dunod, Paris. pp. 110-111."
  (let* ((min (float (/ (car (sort (remove-if #'zerop (mapcar #'third dist-mat)) '<)) 100)))
         (distances (mapcar #'(lambda (x)
				(list (car x) (cadr x) (a-zero (third x) min)))
			    dist-mat))
         (omega (remove-duplicates (append (mapcar #'car distances) (mapcar #'cadr distances)) :test #'equalp))
         (te (list (nth (random (length omega)) omega)))
         (aretes nil)
         (aretes-temp nil)
         (arete-min nil))
    (loop for n from 0
       until (= (length te) (length omega))
       do (setf aretes-temp 'nil)
       (dolist (p te)
         (push (lookformin-if-set p distances te) aretes-temp))
       (setf arete-min (car (sort (remove 'nil aretes-temp) '< :key 'caddr)))
       (setf distances (remove-arete arete-min distances))
       (push arete-min aretes)
       (setf te (add-sommet arete-min te)))
    (mapcar #'(lambda (x) (zeroize x min))
            (remove 'nil aretes))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
