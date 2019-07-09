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

(define (remove-red json-data)
  (cond [(number? json-data) json-data]
        [(and (hash? json-data) (not (hash-has-value? json-data "red")))
         (for/sum ([v (in-hash-values json-data)]) (remove-red v))]
        [(list? json-data)
         (for/sum ([v (in-list json-data)]) (remove-red v))]
        [else 0]))

(define (main)
  (let* ([data-str (read-data)]
         [json-data (parse-json data-str)])
    (printf "Part1: ~a\n" (sum (all-numbers data-str)))
    (printf "Part2: ~a\n" (remove-red json-data))))

(main)