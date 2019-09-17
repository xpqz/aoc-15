#lang racket

;; Advent of Code, day 20 -- 2015
;;
;; https://adventofcode.com/2015/day/20
;;
;; Infinite Elves and Infinite Houses
;;
;; Stefan Kruger


(require math)

(define (presents house)
  (* 10 (divisor-sum house)))

(define (part1 target)
  (for/first ([house (in-naturals 1)]
              #:when (>= (presents house) target)) house))

(define (main)
  (printf "Part1: ~a\n" (part1 29000000)))

(main)