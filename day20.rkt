#lang racket

;; Advent of Code, day 20 -- 2015
;;
;; https://adventofcode.com/2015/day/20
;;
;; Infinite Elves and Infinite Houses
;;
;; Stefan Kruger


(require math)

(define (part1 house)
  (* 10 (divisor-sum house)))

(define (part2 house)
  (* 11 (for/sum ([elf (in-list (divisors house))])
          (if (<= house (* 50 elf)) elf 0))))
                          
(define (find-house target presfn)
  (for/first ([house (in-naturals 1)]
              #:when (>= (presfn house) target)) house))

(define (main)
  (printf "Part1: ~a\n" (find-house 29000000 part1))
  (printf "Part2: ~a\n" (find-house 29000000 part2)))

(main)