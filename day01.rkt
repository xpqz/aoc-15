#lang racket

;; Advent of Code, day 1 -- 2015
;;
;; https://adventofcode.com/2015/day/1
;;
;; Stefan Kruger

(require math)

(define (read-data [filename "data/input01.data"])
  (car (file->lines filename)))

(define (first-negative data)
  (let loop ([pos 0] [floor 0] [data-list (string->list data)])
    (cond [(< floor 0) pos]
          [else
           (cond [(equal? (car data-list) #\() (loop (add1 pos) (add1 floor) (cdr data-list))]
                 [else (loop (add1 pos) (sub1 floor) (cdr data-list))])])))
                  
(define (main)
  (let* ([data (read-data)]
         [brackets (samples->hash (string->list data))]
         [diff (- (hash-ref brackets #\() (hash-ref brackets #\)))])
    (printf "Part1: ~a\n" diff)
    (printf "Part2: ~a\n" (first-negative data))))

(main)