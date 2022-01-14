(defsystem "trie"
  :description "Implementation of a trie/prefix tree"
  :version "1.0.0"
  :author "Jake Grossman <jake.r.grossman@gmail.com>"
  :components ((:file "trie")))

;;;; DEMOS

;; Graphviz demo
(defsystem "trie/graph"
  :build-operation program-op
  :build-pathname "graph"
  :entry-point "graph:toplevel"
  :depends-on ("trie" "unix-opts")
  :components ((:file "examples/graph")))

;; Completion demo
(defsystem "trie/prediction"
  :build-operation program-op
  :build-pathname "prediction"
  :entry-point "prediction:toplevel"
  :depends-on ("trie" "cl-charms")
  :components ((:file "examples/prediction")))
