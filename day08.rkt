#lang racket/load

;; Advent of Code, day 8 -- 2015
;;
;; https://adventofcode.com/2015/day/8
;;
;; Matchsticks
;;
;; Note: in order for the dirty (load ...) hack to work,
;; the datafile had two extra lines added:
;;
;; '(
;; .....
;; )
;;
;; Stefan Kruger

(require math)  ; sum

(define (byte-lines filename)
  (match (file->bytes-lines filename)
    [(list _ middle ... _) middle]))

(define (re-encoded-len byte-str)
  ;; Quotes and backslashes add 2 to the len, plus 2 for the surrounding quotes.
  (+ 2 (for/fold ([len 0]) ([n (in-bytes byte-str)])
         (cond
           [(memq n '(34 92)) (+ len 2)]
           [else (add1 len)]))))

(define (main)
  (let* ([filename "data/input08.data"]
         [bl (byte-lines filename)]
         [b (sum (map bytes-length bl))]
         [s (sum (map string-length (load filename)))]  ; <3 Racket
         [rel (sum (map re-encoded-len bl))])
    (printf "Part1: ~a\nPart2: ~a" (- b s) (- rel b))))

(main)