#lang racket/load

;; Advent of Code, day 8 -- 2015
;;
;; https://adventofcode.com/2015/day/8
;;
;; Matchsticks
;;
;; Note: in order for the dirty (load ..) hack to work,
;; the datafile had three extra characters added:
;;
;; '( ..... )
;;
;; Stefan Kruger

(require math)  ; sum

(define (total-bytes [filename "data/input08.data"])
  (sum (map bytes-length (file->bytes-lines filename))))

(define (total-string-length [filename "data/input08.data"])
  (sum (map string-length (load filename))))

(define (main)
  (let ([b (total-bytes)]
        [s (total-string-length)])
    (printf "Part1: ~a - ~a = ~a\n" b s (- b s 3))))  ; subtract three extra chars added

(main)