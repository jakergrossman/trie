(defpackage :graph
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

(defun run (path)
  (let ((words (uiop:read-file-lines path)))
    (dump-dot (trie:make-trie :initial-contents words))))

(defun toplevel ()
  (let ((path (cond
                ((< (length sb-ext:*posix-argv*) 2)
                 (print "No dictionary file passed, using dict/short.txt..." *error-output*)
                 (print "USAGE: ./graph FILE" *error-output*)
                 (terpri *error-output*)
                 #P"dict/short.txt")
                (t (pathname (nth 1 sb-ext:*posix-argv*))))))
    (run path)))
