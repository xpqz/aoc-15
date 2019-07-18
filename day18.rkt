#lang racket

;; Advent of Code, day 18 -- 2015
;;
;; https://adventofcode.com/2015/day/18
;;
;; Like a GIF For Your Yard
;;
;; Stefan Kruger

(require math)

(define (read-data [filename "data/input18.data"])
  (file->lines filename))

(define (process-row line y)
  (for/set ([x (in-list (map car (regexp-match-positions* #rx"#" line)))])
    (cons x y)))
  
(define (parse-data data)
  (let loop ([row 0] [lines data] [result (set)])
    (if (null? lines)
        result
        (loop (add1 row) (cdr lines) (set-union result (process-row (car lines) row))))))

(define (neighbours s x y)
  (let ([delta '((-1 . -1) (-1 . 0) (-1 . 1) (0 . -1) (0 . 1) (1 . -1) (1 . 0) (1 . 1))])
    (sum (map (λ(d)
       (let ([p (cons (+ x (car d)) (+ y (cdr d)))])
         (if (set-member? s p) 1 0))) delta))))

(define (new-state pos data)
  (match-let* ([`(,x ,y) pos]
               [coord (cons x y)]
               [lit? (set-member? data coord)]
               [n (neighbours data x y)])
    (cond [(and lit? (or (= n 2) (= n 3))) coord]
          [(and (not lit?) (= n 3)) coord]
          [else #f])))

(define (new-state2 pos data)
  (if (member pos '((0 0) (0 99) (99 0) (99 99)))
      (cons (first pos) (second pos))
      (new-state pos data)))

(define (next-gen data statefn)
  (list->set (filter-map (λ (p) (statefn p data)) (cartesian-product (range 100) (range 100)))))

(define (run data count [statefn new-state])
  (let loop ([c count] [state data])
    (cond [(zero? c) (set-count state)]
          [else (loop (sub1 c) (next-gen state statefn))])))
         
(define (main)
  (let* ([data (parse-data (read-data))])
    (printf "Part1: ~a\n" (run data 100))
    (printf "Part2: ~a\n" (run data 100 new-state2))))

(main)