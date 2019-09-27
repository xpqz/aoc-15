#lang racket

(define (value x y)
  (define (next-val v) (remainder (* 252533 v) 33554393))
  (let loop ([row 2] [col 1] [prev 20151125])
    (cond [(and (= row y) (= col x)) (next-val prev)]
          [(= row 1) (loop (add1 col) 1 (next-val prev))]
          [else (loop (sub1 row) (add1 col) (next-val prev))])))
          
(define (main)
  (printf "Part1: ~a\n" (value 3029 2947)))

(main)