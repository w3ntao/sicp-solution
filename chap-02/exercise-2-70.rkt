#lang racket

(require (combine-in (only-in "exercise-2-69.rkt"
                              adjoin-set
                              generate-huffman-tree
                              successive-merge)
                     (only-in "../ToolBox/AbstractionOfData/huffmanTree.rkt"
                              encode)))

(define song-pairs (list (list 'a 2)
                         (list 'na 16)
                         (list 'boom 1)
                         (list 'sha 3)
                         (list 'get 2)
                         (list 'yip 9)
                         (list 'job 2)
                         (list 'wah 1)))

(define song-tree (generate-huffman-tree song-pairs))

;(display (encode '(get a job) song-tree))