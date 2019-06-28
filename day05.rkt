#lang racket

;; Advent of Code, day 5 -- 2015
;;
;; https://adventofcode.com/2015/day/5
;;
;; Doesn't He Have Intern-Elves For This?
;;
;; Stefan Kruger

(require math)       ; samples->hash
(require fancy-app)  ; scala-style lambda syntax

(define (read-data [filename "data/input05.data"])
  (file->lines filename))

(define (has-illegal-substring? str)
  (regexp-match #px"(ab|cd|pq|xy)" str))

(define (has-three-vowels? str)
  (let* ([hist (samples->hash (string->list str))]
         [vowel-count (sum (map (hash-ref hist _ 0) (string->list "aeiou")))])
    (>= vowel-count 3)))

(define (has-double-letter? str)
  (regexp-match #px"(.)\\1" str))
  
(define (nice? str)
  (and
   (not (has-illegal-substring? str))
   (has-three-vowels? str)
   (has-double-letter? str)))

;; part2 procedures

(define (gapped-repeat? str)
  (regexp-match #px"(.).\\1" str))

(define (non-overlapped-pairs? str)
  (regexp-match #px"(..).*\\1" str))

(define (nice-part2? str)
  (and (non-overlapped-pairs? str) (gapped-repeat? str)))
  
(define (main)
  (let* ([data (read-data)]
         [nice-strings (count identity (map nice? data))]
         [nice-strings2 (count identity (map nice-part2? data))])
    (printf "Part1: ~a\nPart2: ~a\n" nice-strings nice-strings2)))
  
(main)