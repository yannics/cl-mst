(in-package :cl-mst)

(defvar *neato* "/opt/local/bin/neato")
(defun expand-path (path)
  (when (string= "~" (subseq path 0 1)) (concatenate 'string (directory-namestring (user-homedir-pathname)) (subseq path 2))))

(defparameter *letters* '(A B C D E F G H I  J K L M N O P Q R S T U V W X Y Z))
(defun int2letter (int &optional res)
  (cond ((zerop int) (apply #'concatenate (cons 'string res)))
	((if (zerop (multiple-value-bind (q r) (floor int 26) r)) (int2letter (1- (floor (/ int 26))) (push "Z" res))))
	(t (int2letter (floor (/ int 26)) (push (write-to-string (nth (multiple-value-bind (q r) (floor (1- int) 26) r) *letters*)) res)))))

(defparameter *day-names* '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defun assoc-color (color-list sequence al &optional stream) ;; |color-list| = |sequence|
  (mapcar #'(lambda (x) (format stream "node [fillcolor=~a] ~{ ~a~};~&" (caar x) (loop for i in x collect (cadr i))))
	  (loop for rrr in (remove-duplicates color-list :test #'equalp) collect (loop for a in (loop for c in color-list for e in sequence collect (list c (cadr (assoc e (mapcar #'reverse al) :test #'equalp)))) when (equalp rrr (car a)) collect a))))
  
(defun neato (mst &key alpha len color scale (path "~/untitled.dot"))
  (let* ((npath (expand-path path))
	 (name (pathname-name npath))
	 (al (when alpha (loop for n in (remove-duplicates (apply #'append (mapcar #'butlast mst)) :test #'equalp) for i from 1 collect (list (int2letter i) n))))
	 (dir (pathname-directory npath)))
    (with-open-file (stream (make-pathname :directory dir
					   :name (if name name "untitled")
					   :type "dot")
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (multiple-value-bind (s m h d mo y a b c) (get-decoded-time)
	(declare (ignore b c s))
	(format stream "/* Generated with cl-mst:neato the ~2,'0d/~2,'0d/~a at ~2,'0d:~2,'0d (~a) */~&" d mo y h m (nth a *day-names*)))
      (format stream "graph ~a {" (if name name "untitled"))
      (if scale
	  (format stream "overlap=scale;~&")
	  (format stream "overlap=false; splines=true;~&"))
      (format stream "node [color=white, style=filled,") 
      (if alpha
	  (format stream " fontname=\"Helvetica bold\", fillcolor=gray28, fontcolor=white];~&")
	  (format stream " fontname=Helvetica, fontsize=65];~&"))
      (when (and alpha color) (assoc-color (mapcar #'car color) (mapcar #'cadr color) al stream)) 
      (dolist (n mst)
	(if alpha
	    (format stream " \"~a\" -- \"~a\"" (cadr (assoc (car n) (mapcar #'reverse al) :test #'equalp)) (cadr (assoc (cadr n) (mapcar #'reverse al) :test #'equalp)))
	    (format stream " \"~a\" -- \"~a\"" (car n) (cadr n)))
	(when len (format stream " [len=~,3F];~&" (caddr n))))
      (format stream "}~&")
      (when alpha
	(format stream "~&/*~&Legend~&")
	(dolist (n al)
	  (format stream "~a ---> ~a~&" (car n) (cadr n)))	
	(format stream "*/~&")))
    (UIOP::run-program (format nil "sh -c '~S -Tpng ~S -o ~S'" *neato* (concatenate 'string (directory-namestring npath) (if name name "untitled") ".dot") (concatenate 'string (directory-namestring npath) (if name name "untitled") ".png")))))
;;;;;;;;;; END
