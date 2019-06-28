#lang racket

;; Advent of Code, day 2 -- 2015
;;
;; https://adventofcode.com/2015/day/2
;;
;; Stefan Kruger

(require racket/match)

(define (read-data [filename "data/input02.data"])
  (file->lines filename))

(define (parse-data data)
  (for/list ([line (in-list data)])
    (map string->number (regexp-match* #px"-?\\d+" line))))

(define (area spec)
  (match-let* ([(list l w h) spec]
               [s1 (* l w)]
               [s2 (* w h)]
               [s3 (* h l)])
    (+ (* 2 s1) (* 2 s2) (* 2 s3) (min s1 s2 s3))))  

(define (ribbon spec)
  (match-let* ([(list l w h) spec]
               [p1 (+ (* 2 l) (* 2 w))]
               [p2 (+ (* 2 l) (* 2 h))]
               [p3 (+ (* 2 h) (* 2 w))]
               [min-perim (min p1 p2 p3)]
               [volume (apply * spec)])
    (+ min-perim volume)))

(define (main)
  (let ([data (parse-data (read-data))])
    (printf "Part1: ~a\n" (apply + (map area data)))
    (printf "Part2: ~a\n" (apply + (map ribbon data)))))

(main)
         