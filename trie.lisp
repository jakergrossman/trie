;;;; trie.lisp
;;;; ~~~~~~~~~
;;;; Implementation of at trie/prefix trie using CLOS.
;;;;
;;;; This is free and unencumbered software released into the public domain.

(defpackage :trie
  (:use :cl)
  (:export :make-trie
           :insert
           :words
           :search-trie
           :prefix
           :suffixes))

(in-package :trie)

(defclass trie ()
  ((prefix
    :initarg :prefix
    :initform nil
    :accessor prefix)
   (suffixes
    :initarg :suffixes
    :initform nil
    :accessor suffixes)))

(defun trunk (trie word)
  "Finds the longest prefix match in TRIE for WORD

Returns two values:
The trie node at the tip of the trunk,
The remainder of the string that could not be matched in TRIE"
  (let* ((char (and (plusp (length word))
                    (char word 0)))
         (next (and char
                    (find char (suffixes trie) :key #'prefix))))
    (cond
      ((null char)
       ;; exhausted string
       (values trie nil))
      ((null next)
       ;; string remaining, but end of match
       (values trie word))
      (t
       ;; more match to process
       (trunk next (subseq word 1))))))

(defun make-suffix (str)
  "Make a unique trie suffix for STR (a chain of individual trie nodes)"
  (if (> (length str) 0)
      ;; More str to process
      (make-instance 'trie :prefix (char str 0)
                           :suffixes (list (make-suffix (subseq str 1))))

      ;; Signal end of word with null trie node
      (make-instance 'trie)))

(defun insert (trie word)
  "Insert WORD into TRIE"
  (multiple-value-bind (trunk rest) (trunk trie word)
    (cond
      ((not (null rest))
       (push (make-suffix rest) (suffixes trunk)))))
  trie)

(defun words (trie &optional stack)
  "Decompose TRIE into a list of words"
  (let ((prefix (slot-value trie 'prefix))
        (suffixes (slot-value trie 'suffixes)))
    (cond
      ;; Word boundary
      ((and (null prefix) (null suffixes))
       (if stack
           (list (coerce (reverse stack) 'string))))
      (t (loop :for suffix :in suffixes
               :append (words suffix (if prefix
                                              (cons prefix stack)
                                              stack)))))))

(defun search-trie (trie prefix)
  "Return every word in TRIE that begins with PREFIX"
  (if (string= prefix "")
      (words trie)
      (multiple-value-bind (trunk rest) (trunk trie prefix)
        (if (null rest)
            (let ((len (1- (length prefix))))
              (mapcar (lambda (x) (concatenate 'string (subseq prefix 0 len) x))
                      (words trunk)))))))

(defun make-trie (&key initial-contents)
  "Create a new trie with INITIAL-CONTENTS"
  (let ((trie (make-instance 'trie)))
    (loop :for word :in initial-contents
          :do (insert trie word))
    trie))

(defun print-trie (stream trie &optional (indent 0))
  (let ((prefix (prefix trie))
        (suffixes (suffixes trie)))
    (cond
      (prefix
       (write-char #\Newline stream)
       (dotimes (i indent) (write-char #\Space stream))
        (format stream "(#\\~c" prefix))
      (t (write-char #\) stream)))

    (loop :for s :in suffixes
          :do (print-trie stream s (+ 2 indent)))))

(defmethod print-object ((obj trie) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((suffixes (suffixes obj)))
      (if suffixes
          (loop :for suffix :in (suffixes obj)
                :do (print-trie stream suffix))
          (write-string "NIL" stream)))))
