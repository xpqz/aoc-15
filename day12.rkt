#lang racket

;; Advent of Code, day 12 -- 2015
;;
;; https://adventofcode.com/2015/day/12
;;
;; JSAbacusFramework.io
;;
;; Stefan Kruger

(require math)  ; sum

(define (read-data [filename "data/input12.data"])
  (car (file->lines filename)))

(define (all-numbers str)
  (map string->number (regexp-match* #px"-?\\d+" str)))

(define (main)
  (let ([data-str (read-data)])
    (printf "Part1: ~a\n" (sum (all-numbers data-str)))))

(main)