;; this file contains random helper functions of general purpose
(defun dre/percent (x y) 
  "computes Y percent of X"
  (interactive)
  ( * x y 0.01))

(defun dre/discount (y x)
 "computes X discounted Y percent"
 (interactive)
 (- x (percent x y)))

(defun dre/increase (y x)
  "computes X increased by Y percent"
  (interactive)
  (+ x (dre/percent x y)))


(defun ajv/bytes-to-human-readable-file-sizes (bytes)
  "Convert number of bytes to human-readable file size."
  (interactive)
  (cond
   ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
   ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
   ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
   ((> bytes 100000) (format "%10.0fk" (/ bytes 1000.0)))
   ((> bytes 1000) (format "%10.1fk" (/ bytes 1000.0)))
   (t (format "%10d" bytes)))
  )
