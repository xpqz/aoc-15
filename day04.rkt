#lang racket

;; Advent of Code, day 4 -- 2015
;;
;; https://adventofcode.com/2015/day/4
;;
;; The Ideal Stocking Stuffer
;;
;; Stefan Kruger

(require openssl/md5)

(define (has-leading-zeros? h count)
  (equal? (~a h #:max-width count) (make-string count #\0)))
   
(define (valid-coin key i leading-zeros)
  (let* ([key-test (format "~a~a" key i)]
         [hashval (md5 (open-input-string key-test))])
    (has-leading-zeros? hashval leading-zeros)))

(define (mine key leading-zeros)
  (for/first ([i (in-naturals)] #:when (valid-coin key i leading-zeros))
    i))
              
(define (main)
  (printf "Part1: ~a\n" (mine "yzbqklnj" 5))
  (printf "Part2: ~a\n" (mine "yzbqklnj" 6)))

(main)
    