#lang racket

;; Advent of Code, day 16 -- 2015
;;
;; https://adventofcode.com/2015/day/16
;;
;; Aunt Sue
;;
;; Stefan Kruger

(require racket/list) ; filter-map

(define target (hash
                "children" `(3 ,=)
                "cats"  `(7 ,>)
                "samoyeds" `(2 ,=)
                "pomeranians" `(3 ,<)
                "akitas"  `(0 ,=)
                "vizslas" `(0 ,=)
                "goldfish" `(5 ,<)
                "trees" `(3 ,>)
                "cars" `(2 ,=)
                "perfumes" `(1 ,=)))

(define (read-data [filename "data/input16.data"])
  (file->lines filename))

(define (sue-data line)
  (let ([sue-number (string->number (second (regexp-match #px"Sue\\s+(\\d+)" line)))])
    `(,sue-number ,(regexp-match* #px"([a-z]+):\\s+(\\d+)" line #:match-select cdr))))

(define (parse-line line)
  (let ([data (sue-data line)])
    (cons (car data)
          (for/set ([m (second data)]) (cons (car m) (string->number (second m)))))))

(define (part1 sues)
  ;; We only care about keys and values. A Sue is a potential target
  ;; if all her keys are present in the target set, with the exact values.
  ;; We can check this as a subset.
  (let ([trg (for/set ([(k v) (in-hash target)]) (cons k (car v)))])
    (filter-map (λ (s) (if (subset? (cdr s) trg) (car s) #f)) sues)))

(define (valid-sue? sue)
  ;; For part2, some keys are special in that their values define upper or lower
  ;; bounds. A comparison function is given in the target hash which
  ;; we then apply. 
  (let loop ([remaining (set->list (cdr sue))])
    (cond [(null? remaining) #t]
          [else
           (let* ([current (car remaining)]
                  [key (car current)]
                  [sue-val (cdr current)]
                  [val&cmp (hash-ref target key #f)])
             (match val&cmp
               [#f #f]
               [(list val cmp)
                (cond [(cmp sue-val val) (loop (cdr remaining))]
                      [else #f])]))])))
    
(define (part2 sues)
  (filter-map (λ (s) (if (valid-sue? s) (car s) #f)) sues))
      
(define (main)
  (let* ([lines (read-data)]
         [sues (map parse-line lines)])
    (printf "Part1: ~a\n" (car (part1 sues)))
    (printf "Part1: ~a\n" (car (part2 sues)))))

(main)