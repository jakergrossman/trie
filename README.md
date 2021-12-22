# etrie
toy implementation of a [Trie/Prefix Tree](https://en.wikipedia.org/wiki/Trie)

## Requirements

- [Steel Bank Common Lisp](http://www.sbcl.org/)
- [Graphviz](https://graphviz.org/)

## Usage

```console
$ sbcl --script etrie.lsp        # save etrie to a standalone executable
$ ./etrie $filename > trie.dot   # using `filename` as a dictionary, create a graph dot file
$ dot -Tsvg trie.dot -O          # generate an SVG for the created graph dot file
```

## Example Output

### Input (included in sample.txt):
```
Helper
Helium
Hello
Fry
Barium
Baritone
Bass
```

### Output (each word highlighted individually):

![Prefix tree for specified input](sample.gif)
