(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "trie.lsp"))

(defpackage :graph
  (:import-from :trie)
  (:use :cl)
  (:export :toplevel))

(in-package :graph)

(defun dump-dot (trie &optional (out *standard-output*))
  "Dump a graph representation of a trie to a dot-file format"
  (let ((next-id 0))
    (labels ((write-out (trie parent)
               (let ((suffixes (trie:suffixes trie)))
                  (loop :for s :in suffixes
                        :for next = (trie:prefix s)
                        :when next
                          :do (incf next-id)
                              (format out "    Node~d [label=\"~c\"]~%" next-id next)
                              (format out "    Node~d -> Node~d~%" parent next-id)
                              (write-out s next-id)))))
      (format out "digraph Trie {~%")
      (format out "    Node0 [label=\"Start\"]~%")
      (write-out trie 0)
      (format out "}~%"))))

(defun run (words)
  (dump-dot (trie:make-trie words)))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (cond
    ((< (length sb-ext:*posix-argv*) 2)
     (print "No dictionary file specified!")
     (terpri))
    (t
     (let* ((words (uiop:read-file-lines (nth 1 sb-ext:*posix-argv*))))
       (if words
           (run words)
           (format t "Could not open dictionary file '~a'~%" (nth 1 sb-ext:*posix-argv*)))))))
