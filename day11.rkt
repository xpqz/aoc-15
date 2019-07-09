#lang racket

;; Advent of Code, day 11 -- 2015
;;
;; https://adventofcode.com/2015/day/11
;;
;; Corporate Policy
;;
;; Stefan Kruger

(define (rot1 l)
  (integer->char (+ (modulo (add1 (- (char->integer l) 97)) 26) 97)))

(define (next-password password)
  (let loop ([new '()][current (reverse (string->list password))])
    (cond [(null? current) (list->string (reverse new))]
          [else
           (let ([new-letter (rot1 (car current))])
             (match new-letter
               [#\a (loop (cons #\a new) (cdr current))]
               [_ (list->string (append (reverse (cdr current)) `(,new-letter) new))]))])))

(define (consecutive-run? password)
  ;; abc bcd cde etc
  (regexp-match
   #px"(abc|bcd|cde|def|efg|fgh|ghi|hij|ijk|jkl|klm|lmn|mno|nop|opq|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz)"
   password))

(define (illegal-letters? password)
  ;; i o l
  (regexp-match #px"[iol]" password))

(define (non-overlapping-pairs? password)
  ;; At least two different pairs of the same letter: aa bb cc etc
  (> (set-count (list->set (regexp-match* #px"(.)\\1" password))) 1))

(define (valid-password? password)
  (and
   (consecutive-run? password)
   (not (illegal-letters? password))
   (non-overlapping-pairs? password)))

(define (find-next-password password)
  (let loop ([candidate (next-password password)])
    (cond [(valid-password? candidate) candidate]
          [else
           (loop (next-password candidate))])))
                
(define (main)
  (let ([part1 (find-next-password "hepxcrrq")])
    (printf "Part1: ~a\n" part1)
    (printf "Part2: ~a\n" (find-next-password part1))))

(main)