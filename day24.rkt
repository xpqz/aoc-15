#lang racket

;; Part brute-force, part guesswork.
;;
;; Find increasing partition lengths that make up a third of the total.
;; Assume that we only need to consider a small number of partition lengths.

(require math) ;; sum

(define data '(1 3 5 11 13 17 19 23 29 31 37 41 43 47 53 59 67 71 73 79 83 89 97 101 103 107 109 113))

(define (find-partitions d s)
  (define val (/ (sum d) s))
  (define best +inf.0)
  (for*/fold ([res '()])
             ([n (in-range 1 8)]
              [valid (in-combinations d n)] #:when (= (sum valid) val))
    (define v (apply * valid))
    (cond [(< v best) (set! best v) (cons valid res)]
          [else res])))
    
(define (main)
  (printf "Part1: ~a\n" (apply * (first (find-partitions data 3))))
  (printf "Part2: ~a\n" (apply * (first (find-partitions data 4)))))

(main)
