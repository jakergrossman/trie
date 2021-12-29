;;;; NCURSES Prediction Demo
;;;;
;;;; Displays list of predicted words based on user typed word

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "trie.lsp")
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (cond
      ((probe-file quicklisp-init)
       (load quicklisp-init)
       (ql:quickload :cl-charms :silent t)
       (ql:quickload :unix-opts :silent t))
      (t
       (format *error-output* "[ERROR] Quickload 'setup.lisp' not present in $HOME/quicklisp")
       (sb-ext:exit)))))

(defpackage :prediction
  (:use :cl)
  (:export :toplevel))

(in-package :prediction)

(defparameter *words* nil)              ; dictionary of words
(defparameter *trie* (trie:make-trie))  ; trie structure representing `*words*`
(defparameter *file* nil)               ; file to load words from
(defvar *input* "")                     ; current value of the user input
(defvar *prev-input* nil)
(defvar *match-list* nil)               ; list of matches in `*trie*` that are prefixed by `*input*`

(defun paint/string (str x y &key centered)
  "Draw an optionally centered string STR at row Y and column X"
  (let ((effective-x (if centered (- x (floor (length str) 2)) x)))
    (charms:write-string-at-point charms:*standard-window* str effective-x y)))

(defun paint/controls ()
  "Draw the controls for the program"
  (multiple-value-bind (width height)
      (charms:window-dimensions charms:*standard-window*)
    (let* ((y (floor height 4))
           (x1 (floor width 4))
           (x2 (* 3 x1))
           (x3 (* 2 x1)))
      (paint/string "<esc> - quit" x1 y :centered t)
      (paint/string "Type to see autocompletions" x3 (- y 4) :centered t)
      (paint/string
       (format nil "~d words from '~a'" (length *words*) *file*) x3 (- y 2) :centered t)
      (paint/string "<enter> - clear input" x2 y :centered t))))

(defun paint/completions ()
  "Draw the number and list of completions"
    (multiple-value-bind (width height)
        (charms:window-dimensions charms:*standard-window*)
      (let ((left (* 2 (floor width 5)))
            (num-completions (write-to-string (if *match-list* (length *match-list*) 0))))
        (charms:write-string-at-point charms:*standard-window*
                                      (format nil "(~d)" num-completions)
                                      (- left 3 (length num-completions))
                                      (floor height 2))
          (loop for y from (+ 2 (floor height 2)) below height
                for i from 0
                for word = (if (< i (length *match-list*)) (nth i *match-list*))
                when word
                  do (charms:write-string-at-point charms:*standard-window* word left y)))))

(defun paint/input ()
  "Draw user input"
  (multiple-value-bind (width height)
      (charms:window-dimensions charms:*standard-window*)
  (let ((left (* 2 (floor width 5))))
    (charms:write-string-at-point charms:*standard-window* *input* left (floor height 2)))))

(defun orderp (a b)
  "Sort by alphabetical order, breaking ties by length"
  (or (string< a b)
      (< (length a) (length b))))

(defun paint/update ()
  "Update word list and paint items to the screen"
  (when (not (equal *prev-input* *input*))
    (setf *match-list* (sort (trie:search-trie *trie* *input*) #'orderp))
    (setf *prev-input* *input*))

  (paint/controls)
  (if (> (length *input*) 0)
      (paint/completions))
  (paint/input))

(defun reset ()
  "Reset program state"
  (setf *input* ""
        *prev-input* nil
        *match-list* nil))

(defun handle-keyboard (c)
  "Handles a single keypress C. Returns T when escape is pressed, otherwise NIL."
  (case c
    ((#\Backspace #\Rubout)
     (when (> (length *input*) 0)
       (setf *input* (subseq *input* 0 (1- (length *input*))))))

    ((#\Return #\Newline) (reset))

    (otherwise
     (when (and c (graphic-char-p c))
       (setf *input* (concatenate 'string *input* (string c))))))

  ;; exit iff pressed character is escape
  (and c (char= c #\Esc)))

(opts:define-opts
  (:name :help
   :description "Print this help text."
   :short #\h
   :long "help")
  (:name :dict
   :description "The dictionary file to use when building the Trie."
   :short #\d
   :long "dict"
   :default "dict/long.txt"
   :arg-parser #'identity
   :meta-var "DICTIONARY"))

(defun help ()
  (opts:describe
   :prefix (with-output-to-string (out)
             (write-string
              "prediction - present a list of autocomplete suggestions for user input"
              out)
             (write-char #\Newline out)
             (write-char #\Newline out)
             (write-string "Usage: prediction [-h] [--dict DICTIONARY]" out)
             (write-char #\Newline out))))

(defun toplevel ()
  "Run the autocomplete demo"
  (sb-ext:disable-debugger)
  (let ((options (opts:get-opts)))
    (when (getf options :help)
      (help)
      (sb-ext:exit))

    (setf *file* (getf options :dict))
    (setf *words* (uiop:read-file-lines *file*))
    (reset)
    (loop for w in *words* do (trie:insert *trie* w))

    (charms:with-curses ()
      (charms:disable-echoing)
      (charms:enable-raw-input)
      (charms:enable-non-blocking-mode charms:*standard-window*)

      (loop named driver
            for c = (charms:get-char charms:*standard-window*
                                     :ignore-error t)
            do (progn
                 (when (handle-keyboard c)
                   (return-from driver))
                 (charms:clear-window charms:*standard-window*)
                 (paint/update)
                 (charms:refresh-window charms:*standard-window*)
                 (sleep 1/60))))))
