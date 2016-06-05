#lang racket

(require (only-in "../ToolBox/AbstractionOfData/stream.rkt"
                  integers
                  pairs
                  display-stream-until))

(display-stream-until (pairs integers
                             integers)
                      198)