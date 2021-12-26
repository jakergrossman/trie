;;;; trie.lsp
;;;; Toy implementation of a trie
;;;;
;;;; This is free and unencumbered software released into the public domain.

(defpackage :trie
  (:use :cl)
  (:export :make-trie
           :insert
           :trie-words
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

(defun find-trunk (trie trunk)
  "Finds the trie branch that has the longest-prefix-match with `trunk`"
  (let* ((char (if (> (length trunk) 0) (char trunk 0)))
         (next (if char (find char (suffixes trie) :key #'prefix))))
    (cond
      ((null char)
       (values trie nil))
      ((null next)
       (values trie trunk))
      (t (find-trunk next (subseq trunk 1))))))

(defun make-suffix (word)
  "Make the suffix portion of a new word"
  (if (> (length word) 0)
      ;; More word to process
      (make-instance 'trie :prefix (char word 0)
                           :suffixes (list (make-suffix (subseq word 1))))

      ;; Signal end of word with null trie node
      (make-instance 'trie)))

(defun insert (trie word)
  "Insert a new word into a trie"
  (multiple-value-bind (trunk rest) (find-trunk trie word)
    (cond
      ((not (null rest))
       (push (make-suffix rest) (suffixes trunk)))))
  trie)

(defun trie-words (trie &optional stack)
  "Find every word in trie"
  (let ((prefix (slot-value trie 'prefix))
        (suffixes (slot-value trie 'suffixes)))
    (cond
      ;; Word boundary
      ((and (null prefix) (null suffixes))
       (if stack
           (list (coerce (reverse stack) 'string))))
      (t (loop :for suffix :in suffixes
               :append (trie-words suffix (if prefix
                                              (cons prefix stack)
                                              stack)))))))

(defun search-trie (trie prefix)
  "Find every word in `trie` that starts with `prefix`"
  (if (string= prefix "")
      (trie-words trie)
      (multiple-value-bind (trunk rest) (find-trunk trie prefix)
        (if (null rest)
            (let ((len (1- (length prefix))))
              (mapcar (lambda (x) (concatenate 'string (subseq prefix 0 len) x))
                      (trie-words trunk)))))))

(defun make-trie (&optional words)
  "Create a new trie initialized with `words`"
  (let ((trie (make-instance 'trie)))
    (loop :for word :in words
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
