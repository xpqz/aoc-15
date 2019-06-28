#lang racket

;; Advent of Code, day 3 -- 2015
;;
;; https://adventofcode.com/2015/day/3
;;
;; Perfectly Spherical Houses in a Vacuum
;;
;; Stefan Kruger

(require racket/match)
(require racket/hash)

(define (read-data [filename "data/input03.data"])
  (string->list (first (file->lines filename))))

(define (partition lst)
  (for/fold ([evens '()] [odds '()] #:result (list (reverse evens) (reverse odds)))
             ([pair (in-slice 2 lst)])
    (match pair
      [(list a b) (values (cons a evens) (cons b odds))]
      [(list a) (values (cons a evens) odds)])))

(define (move pos ch)
  (match-let ([(cons x y) pos])
    (case ch
      [(#\<) (cons (sub1 x) y)]
      [(#\>) (cons (add1 x) y)]
      [(#\^) (cons x (add1 y))]
      [(#\v) (cons x (sub1 y))])))
               
(define (visited-houses data)
  (for/fold ([visited (hash (cons 0 0) 1)]
             [pos (cons 0 0)] #:result visited)
            ([ch (in-list data)])
    (let ([new-pos (move pos ch)])
      (values (hash-update visited new-pos add1 0) new-pos))))

(define (visited-houses-with-robo-santa data)
  (match-let* ([(list santa-moves robo-santa-moves) (partition data)]
               [santa-visits (visited-houses santa-moves)]
               [robo-santa-visits (visited-houses robo-santa-moves)])
    (hash-union santa-visits robo-santa-visits #:combine/key (λ (k a b) (+ a b)))))

(define (main)
  (let ([data (read-data)])
    (printf "Part1: ~a\n" (hash-count (visited-houses data)))
    (printf "Part2: ~a\n" (hash-count (visited-houses-with-robo-santa data)))))

(main)
    