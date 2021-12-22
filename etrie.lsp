; etrie.lsp
; Toy implementation of a trie
;
; This is free and unencumbered software released into the public domain.

; TODO: Modify in-place maybe
(defun insert-into-trie (base &rest words)
  "Insert a new word into a trie, returning the updated trie"
  (loop :for word :in words
        :do (cond
              ((> (length word) 0)
               (let* ((letter (char word 0))
                      (rest (subseq word 1))
                      (entry (assoc letter base)))
                 (cond
                   ((null entry)
                    (push (cons letter (insert-into-trie nil rest)) base))
                   (t (setf (cdr entry)
                            (insert-into-trie (cdr entry) rest))))))))
  base)

(defun trie-words (trie &optional stack)
  "Find every word in trie"
  (cond
    ((null trie) (list (coerce (reverse stack) 'string)))
    (t (loop :for (char . children) :in trie
             :append (trie-words children (cons char stack))))))

(defun search-trie (trie prefix)
  "Find every word in `trie` that starts with `prefix`"
  (labels ((find-stems (trie prefix)
             (cond
               ((= (length prefix) 0) (trie-words trie))
               (t (let* ((char (char prefix 0))
                         (next (assoc char trie)))
                    (cond
                      ((null next) nil)
                      (t (find-stems (cdr next) (subseq prefix 1)))))))))
    (let ((stems (find-stems trie prefix)))
      (mapcar (lambda (x) (concatenate 'string prefix x)) stems))))

(defun dump-dot (trie &optional (out *standard-output*))
  "Dump a graph representation of a trie to a dot-file format"
  (let ((next-id 0))
    (labels ((write-out (trie parent)
                (loop :for entry :in trie
                      :for id = (incf next-id)
                      :do (format out "    Node~d [label=\"~c\"]~%" id (car entry))
                          (format out "    Node~d -> Node~d~%" parent id)
                          (write-out (cdr entry) id))))
      (format out "digraph Trie {~%")
      (format out "    Node0 [label=\"Start\"]~%")
      (write-out trie 0)
      (format out "}~%"))))

(defun get-lines (file)
  "Read the lines of a file as an array of strings"
  (let ((lines (with-open-file (stream file)
                 (loop :for line = (read-line stream nil nil)
                       :while line
                       :collect line))))
    (make-array (list (length lines)) :initial-contents lines)))

(defun main ()
  (cond
    ((< (length sb-ext:*posix-argv*) 2)
     (print "No dictionary file specified!")
     (terpri))
    (t
      (let* ((words (get-lines (nth 1 sb-ext:*posix-argv*)))
             (trie nil))
        (loop :for n :from 0 :by 35
              :for remaining = (length words) :then (- remaining 35)
              :for batch-size = (min remaining 35)
              :until (>= n (length words))
              :do (setf trie (apply #'insert-into-trie trie
                                    (coerce (subseq words n (+ n batch-size)) 'list))))
        (dump-dot trie)))))

(sb-ext:save-lisp-and-die #P"etrie" :toplevel #'main :executable t)
