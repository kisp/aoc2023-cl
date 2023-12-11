;;; This program uses screamer - https://cliki.net/screamer
#+nil (ql:quickload "screamer")

(in-package :screamer-user)

(defvar *input*
  '("467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598.."))

(defun read-input (&optional (pathname "/tmp/input.txt"))
  (setq *input*
        (with-open-file (in pathname)
          (loop for line = (read-line in nil)
                while line
                collect line)))
  'done)

#+nil (read-input)

(defun pos-char (pos)
  (destructuring-bind (row column) pos
    (char (nth row *input*) column)))

(defmacro with-pos (vars pos &body body)
  (ecase (length vars)
    (2 `(destructuring-bind ,vars ,pos
          ,@body))
    (3 `(let ((,(car vars) ,pos))
          (destructuring-bind ,(cdr vars) ,(car vars)
            ,@body)))))

(defun symbol-char-p (char)
  (and (not (digit-char-p char))
       (char/= char #\.)))

(defun a-row ()
  (an-integer-between 0 (1- (length *input*))))

(defun a-column ()
  (an-integer-between 0 (1- (length (first *input*)))))

(defun a-rowv ()
  (an-integer-betweenv 0 (1- (length *input*))))

(defun a-columnv ()
  (an-integer-betweenv 0 (1- (length (first *input*)))))

(defun a-pos ()
  (list (a-row) (a-column)))

(defun a-posv ()
  (list (a-rowv) (a-columnv)))

(defun a-pos-with-char ()
  (let ((pos (a-pos)))
    (list pos (pos-char pos))))

(defun a-pos-with-char-satisfying (predicate)
  (destructuring-bind (pos char)
      (a-pos-with-char)
    (unless (funcall predicate char)
      (fail))
    pos))

(defun a-pos-satisfying (predicate)
  (let ((pos (a-pos)))
    (unless (funcall predicate pos)
      (fail))
    pos))

(defun a-pos-with-column-offset (pos offset)
  (with-pos (r c) pos
    ;; This slows it down quite a bit
    #+nil
    (with-pos (new-pos nr nc) (a-pos)
      (unless (eql r nr) (fail))
      (unless (eql (+ c offset) nc) (fail))
      new-pos)
    (with-pos (new-pos nr nc) (a-posv)
      (assert! (=v r nr))
      (assert! (=v (+v c offset) nc))
      (solution new-pos (static-ordering #'linear-force)))))

(defun a-pos-with-row-column-offset (pos row-offset column-offset)
  (with-pos (r c) pos
    (with-pos (new-pos nr nc) (a-posv)
      (assert! (=v (+v r row-offset) nr))
      (assert! (=v (+v c column-offset) nc))
      (solution new-pos (static-ordering #'linear-force)))))

(defun a-number-starting-pos ()
  (let ((pos (a-pos-with-char-satisfying #'digit-char-p)))
    (let ((pos-before
            (one-value
                (a-pos-with-column-offset pos -1)
                nil)))
      (when (and pos-before
                 (digit-char-p (pos-char pos-before)))
        (fail)))
    pos))

(defun a-pos-to-the-right-of (pos)
  (a-pos-with-column-offset
   pos
   (an-integer-abovev 1)))

(defun a-pos-from-left-to-right (left-pos right-pos)
  (with-pos (lr lc) left-pos
    #+nil (declare (ignore lr)) lr
    (with-pos (rr rc) right-pos
      #+nil (declare (ignore rr)) rr
      (a-pos-with-column-offset
       left-pos
       (an-integer-betweenv 0 (- rc lc))))))

(defun cells-from-left-to-right-satisfy (left-pos right-pos predicate)
  (necessarily?
    (let ((pos (a-pos-from-left-to-right left-pos right-pos)))
      (funcall predicate (pos-char pos)))))

(defun a-pos-of-number-starting-at (left-pos)
  (let ((pos (either left-pos
               (a-pos-to-the-right-of left-pos))))
    (unless (digit-char-p (pos-char pos))
      (fail))
    (unless (cells-from-left-to-right-satisfy
             left-pos pos
             #'digit-char-p)
      (fail))
    pos))

(defun right-pos-of-number-starting-at (left-pos)
  (car
   (last
    (all-values
      (a-pos-of-number-starting-at left-pos)))))

(defun an-adjacent-pos (pos)
  (let ((row-offset (an-integer-betweenv -1 1))
        (column-offset (an-integer-betweenv -1 1)))
    (assert! (notv
              (andv (=v row-offset 0)
                    (=v column-offset 0))))
    (a-pos-with-row-column-offset
     pos row-offset column-offset)))

(defun number-is-adjacent-to-symbol (left-pos)
  (possibly?
    (let ((pos
            (an-adjacent-pos
             (a-pos-of-number-starting-at left-pos))))
      (symbol-char-p (pos-char pos)))))

(defun number-to-integer (left-pos)
  (parse-integer
   (format
    nil "~{~C~}"
    (all-values
      (let ((pos (a-pos-of-number-starting-at left-pos)))
        (pos-char pos))))))

(defun part-a ()
  (let ((numbers
          (all-values
            (let ((pos (a-number-starting-pos)))
              (unless (number-is-adjacent-to-symbol pos)
                (fail))
              (number-to-integer pos)))))
    (values numbers
            (reduce #'+ numbers))))
