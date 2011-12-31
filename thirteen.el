; -*- Emacs Lisp -*-
;
; Neil deGrasse Tyson tweet 12/7/2011: Need a distraction today? Not
; only does 12+1=11+2, but the letters "twelve plus one" rearrange to
; give you "eleven plus two"
;
; Sound like a Programming Praxis problem, anyone?
;
; Ugly solution below searches +, -, *, / up to 13, in Emacs Lisp for
; practice. Looping constructs, instead of recursion, avoid blowing
; out Emacs stack limits.  I estimate O( 2.5 * N^2 ).
;
; mitchell.perilstein@gmail.com
;

(defconst GENMAX   13)
(defconst OPWORDS  '((+ plus) (- minus) (* times) (/ divide)))
(defconst NUMWORDS '(zero one two three four five six seven eight nine ten eleven twelve thirteen ))

(defun math-words-equal (a b)
  (equal (sorted-explode (equation-to-mathwords a))
	 (sorted-explode (equation-to-mathwords b))))

(defun numword (n) (symbol-name (nth n NUMWORDS)))
(defun opword (o) (symbol-name (cadr (assoc o OPWORDS))))
(defun sorted-explode (str) (sort (coerce str 'list) '<))

(defun equation-to-mathwords (e) 
  (concat (numword (cadr e)) " " (opword (car e)) " " (numword (caddr e))))

(defun equations-equal (x y)
  (and (not (equal x y))
       (equal (eval x) (eval y)) 
       (math-words-equal x y)))

(defun generate-equations ()      ;; ((- 5 5) (+ 5 5) (- 5 4) ....
  (let ((e nil)
	(ops (mapcar 'car OPWORDS)))
    (loop for i from 1 to GENMAX do 
	  (loop for j from 1 to i do 
		(loop for op in ops do
		      (if (not (and (equal op '-) (equal i j))) ;;; no 0's please
			  (setq e (cons (list op i j) e))))))
    e))

(defun thirteen ()
  (let ((all (generate-equations))
	(out nil))
    (loop for i from 1 to (length all) do
	  (loop for j from 1 to i do
		(let ((a (nth i all))
		      (b (nth j all)))
		  (if (equations-equal a b)
		      (setq out (cons (list 
				       a (equation-to-mathwords a) 
				       b (equation-to-mathwords b))
				      out))))))
    out))

;; (thirteen)
;; (((+ 11 2) "eleven plus two" (+ 12 1) "twelve plus one"))
