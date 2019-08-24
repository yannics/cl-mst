(in-package :cl-mst)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preliminary note

;; cl-mst algorithms require as argument:
;; a list of weighted edges as a matrix.

;; The matrix is a list of pairs of nodes as edges of a graph
;; with their respective weight as a distance for instance.
;; The matrix is formated as follow:
;; ((n1 n2 w12) (n1 n3 w13) ... )

;;------------------------------------------------------------------
;;                                                    READ-DATA-FILE

(defun read-text-lines (file)
  (with-open-file (in-stream file
			     :direction :input
                             :element-type 'character)
    (loop with length = (file-length in-stream)
       while (< (file-position in-stream) length)
       collect (read-line in-stream))))

(defun string-to-list (string)
  (let ((the-list nil) 
        (end-marker (gensym)))
    (loop (multiple-value-bind (returned-value end-position)
	      (read-from-string string nil end-marker)
            (when (eq returned-value end-marker)
              (return the-list))
            (setq the-list 
                  (append the-list (list returned-value)))
            (setq string (subseq string end-position))))))

(defun read-file (file)
  (loop for i in (read-text-lines file) collect (string-to-list (string-trim '(#\Space #\Tab #\Newline) i))))

;;------------------------------------------------------------------
;;                                                      WRITE-MATRIX

(defvar *g++* (merge-pathnames "src/cl-mst.sh" (asdf:system-source-directory :cl-mst)))
(UIOP::run-program (format nil "sh -c '~a init'" *g++*))
(load (format nil "~a~(~a~)" (directory-namestring *g++*) '.tmp))

(defun write-matrix (matrix)
  (with-open-file (stream (make-pathname :directory (pathname-directory *g++*)
					 :name ".tmp")
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "int main() {~2&")
    (format stream "   int V = ~S;~&" (length (remove-duplicates (loop for i in matrix append (butlast i)))))
    (format stream "   int E = ~S;~2&" (length matrix))
    (format stream "   Graph *graph = createGraph(V, E);~2&")
    (loop for i in matrix for x from 0 do
	 (format stream "graph->edge[~a].source = ~a;~&" x (car i))
	 (format stream "graph->edge[~a].destination = ~a;~&" x (cadr i))
	 (format stream "graph->edge[~a].weight = ~a;~2&" x (caddr i)))
    (format stream "   boruvkaMST(graph);~2&")
    (format stream "   return(0);~&")
    (format stream "}~&")))

;;------------------------------------------------------------------

(defparameter *mat* (make-hash-table :test #'equalp))
(defun >i (matrix)
  (clrhash *mat*)
  (loop for el in (remove-duplicates (loop for e in matrix append (butlast e))) for i from 0 do (setf (gethash el *mat*) i))
  (loop for m in matrix collect (list (gethash (car m) *mat*) (gethash (cadr m) *mat*) (caddr m))))

(defun <i (mst)
  (let ((al (loop for value being the hash-values of *mat*
	       using (hash-key key)
	       collect (list value key))))
    (loop for m in mst collect (list (cadr (assoc (car m) al)) (cadr (assoc (cadr m) al)) (caddr m)))))

(defvar *mst* nil)
(defun boruvka (graph &key (about 3))
  (write-matrix (loop for i in (>i graph) collect (append (butlast i) (list (round (* (expt 10 about) (caddr i)))))))
  (UIOP::run-program (format nil "sh -c '~a run'" *g++*))
  (setf *mst* (read-file (format nil "~a~(~a~)" (directory-namestring *g++*) '.tmp)))
  (loop for i in (<i *mst*) collect (append (butlast i) (list (* 1.0 (/ (caddr i) (expt 10 about)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code from library OpenMusic/PWGL fv-morphologie-20111205
;; source: http://www.fredvoisin.com/spip.php?article113
;;;;;;;;;; measuring mst ...
;; Note that the term rhizome to name the following functions seems
;; more appropriate than tree or graph and it echoes to the reference
;; to Deleuze previously cited with the function a-zero

(defun rhizome-length (tree)
  "Sum of all edge's length of the tree."
  (apply #'+ (mapcar #'caddr tree)))

(defun rhizome-degree (node tree)
  "Degree of the node in the tree."
  (length (remove-if-not #'(lambda (x) (member node (butlast x) :test #'equalp)) tree)))

(defun rhizome-nodes (tree &optional (min-degree 2))
  "List of nodes in a tree according to a minimal degree (2 by default)."
  (let* ((e  (mapcar #'butlast (remove-if #'(lambda (x) (zerop (caddr x))) tree)))  ;; remove if dist = 0
         (v (remove-duplicates (apply #'append e) :test #'equalp))
         (r (list)))
    (dolist (k v)
      (push (list k (remove-if-not #'(lambda (x) (member k x :test #'equalp)) e)) r))
    (mapcar #'car (remove-if #'(lambda (x) (< (length (cadr x)) min-degree)) r))))

(defun rhizome-leaves (tree)
  "List only leaves in a tree, meaning all nodes with a degree equal to one."
  (let* ((e  (mapcar #'butlast tree))
         (v (remove-duplicates (apply #'append e)))
         (r (list)))
    (dolist (k v)
      (push (list k (remove-if-not #'(lambda (x) (member k x :test #'equalp)) e)) r))
    (mapcar #'car (remove-if-not #'(lambda (x) (= (length (cadr x)) 1)) r))))

(defun flat-1 (liste)
  (if (consp (car liste))
      (apply 'append liste)
      liste))

(defun remove-ifnot-1 (set)
  (remove-if #'(lambda (s) (not (= 1 (length s)))) set))

(defun remove-ifonly-1 (set)
  (remove-if #'(lambda (s) (= 1 (length s))) set))

(defun extremites (points tree)
  (mapcar #'car
	  (remove-ifnot-1
	   (mapcar #'(lambda (a) (remove 'nil a))
		   (mapcar #'(lambda (point)
                               (mapcar #'(lambda (x)
                                           (when (not (equalp 'nil (member point x))) x))
                                       tree))
			   points)))))

(defun lul (l1 l2)
  (cond ((equalp (car (last l1)) (car l2)) (append (butlast l1) l2))
	((equalp (car (last l1)) (cadr l2)) (append (butlast l1) (reverse l2)))
	(t (lul (reverse l1) l2))))

(defun lul1 (ldl)
  (if (null (cdr ldl))
      (flat-1 ldl)
      (lul1 (append (list (lul (car ldl) (cadr ldl))) (cddr ldl)))))

(defun topotree (tree)
  (setf tree (mapcar #'butlast tree))
  (let ((points (remove-duplicates (append (mapcar #'car tree) (mapcar #'cadr tree)) :test #'equalp))
	(non-extr (copy-list tree))
	noeuds extremites)
    (setf extremites (extremites points tree))
    (dolist (x extremites) (setf non-extr (remove x non-extr :test #'equalp)))
    (setf non-extr (remove-duplicates (flat-1 non-extr) :test #'equalp)
	  extremites (remove-duplicates (flat-1 extremites) :test #'equalp))
    (dolist (x non-extr)
      (setf extremites (remove x extremites :test #'equalp)))
    (setf noeuds
	  (remove-ifonly-1
	   (mapcar #'(lambda (a) (remove 'nil a))
		   (mapcar #'(lambda (point) (mapcar #'(lambda (x) (when (not (equalp 'nil (member point x))) x)) tree))
			   points))))
    (values extremites noeuds)))

(defun extr-to-extr (start extr noeuds r count)
  (let ((copy-of-noeuds (copy-tree noeuds)))
    (loop for i from 0 to (1- (length copy-of-noeuds))
       do (dotimes (j (length (nth i copy-of-noeuds)))
	    (when (and (not (member (nth j (nth i copy-of-noeuds)) r :test #'equalp))
		       (member start (nth j (nth i copy-of-noeuds)) :test #'equalp))
	      (push (nth j (nth i copy-of-noeuds)) r)
	      (setf start (car (remove-if #'(lambda (x) (eq start x)) (nth j (nth i copy-of-noeuds))))
		    (nth j (nth i copy-of-noeuds)) 'nil
		    count 0))))
    (cond ((and (not (member start extr)) (zerop count))
	   (extr-to-extr start extr copy-of-noeuds r (1+ count)))
	  (t (list r start count)))))

(defun extr-to-extr2 (start extr noeuds r)
  (if (null extr)
      (mapcar #'reverse r)
      (let ((p (extr-to-extr start extr noeuds 'nil 0)))
	(cond ((> (car (last p)) 0)
	       (setf noeuds (mapcar #'(lambda (x) (remove-if #'(lambda (a) (member (cadr p) a)) x)) noeuds)
		     extr (remove (cadr p) extr))
	       (extr-to-extr2 start extr noeuds r))
	      (t (push (car p) r)
		 (setf noeuds
		       (mapcar #'(lambda (x) (remove (caar r) x :test #'equalp)) noeuds)
		       extr (remove (cadr p) extr))
		 (extr-to-extr2 start extr noeuds r))))))

(defun extr-to-extr3 (start extr noeuds)
  (extr-to-extr2 start extr noeuds 'nil))

(defun rhizome-path (node1 node2 tree)
  (multiple-value-bind (extremites noeuds)
      (topotree tree)
    (let ((long-path
	   (car (remove-if-not #'(lambda (x) (and (member node1 x :test #'equalp) (member node2 x :test #'equalp)))
			       (mapcar #'lul1 (extr-to-extr3 node1 extremites noeuds))))))
      (subseq long-path 0 (1+ (position node2 long-path :test #'equalp))))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
