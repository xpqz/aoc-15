#lang racket

;; Advent of Code, day 12 -- 2015
;;
;; https://adventofcode.com/2015/day/12
;;
;; JSAbacusFramework.io
;;
;; Stefan Kruger

(require math)  ; sum
(require json)

(define (read-data [filename "data/input12.data"])
  (car (file->lines filename)))

(define (all-numbers str)
  (map string->number (regexp-match* #px"-?\\d+" str)))

(define (parse-json str)
  (with-input-from-string str (λ () (read-json))))

(define (hash-has-value? haystack needle)
  (set-member? (list->set (hash-values haystack)) needle))

(define (remove-red json-data [total 0])
  (cond [(number? json-data) (+ total json-data)]
        [(and (hash? json-data) (not (hash-has-value? json-data "red")))
         (+ total (sum (for/list ([v (in-hash-values json-data)]) (remove-red v total))))]
        [(list? json-data)
         (+ total (sum (for/list ([v (in-list json-data)]) (remove-red v total))))]
        [else total]))

(define (main)
  (let* ([data-str (read-data)]
         [json-data (parse-json data-str)])
    (printf "Part1: ~a\n" (sum (all-numbers data-str)))
    (printf "Part2: ~a\n" (remove-red json-data))))

(main)