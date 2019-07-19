#lang racket

;; Advent of Code, day 19 -- 2015
;;
;; https://adventofcode.com/2015/day/19
;;
;; Medicine for Rudolph
;;
;; Stefan Kruger

(require math)

(define (read-data [filename "data/input19.data"])
  (file->lines filename))

(define (parse-data lines)
  (let loop ([rules '()] [data lines])
    (match-let ([(list line rst ...) data])
      (match (string-split line)
        [(list molecule) (cons rules molecule)]
        ['() (loop rules rst)]
        [(list target "=>" replacement) (loop (cons (cons target replacement) rules) rst)]))))
    
(define (apply-rule molecule rule)
  (let loop ([prefix ""] [results '()] [matches (string-split molecule (car rule) #:trim? #f)])
    (match matches
      [(list _) results]
      [_ (match-let* ([(list this rest ...) matches]
                      [tail-str (string-join rest (car rule))]
                      [new-string (string-join (append `(,prefix ,this ,(cdr rule) ,tail-str)) "")]
                      [new-prefix (string-join `(,prefix ,this ,(car rule)) "")])
           (loop new-prefix (cons new-string results) rest))])))

(define (unique-molecules data)
  (set-count (list->set (append-map (curry apply-rule (cdr data)) (car data)))))

(define (main)
  (let* ([data (parse-data (read-data))])
    (printf "Part1: ~a\n" (unique-molecules data))))
    
(main)