#lang racket

;; Advent of Code, day 9 -- 2015
;;
;; https://adventofcode.com/2015/day/9
;;
;; All in a Single Night
;;
;; Stefan Kruger

(define (read-data [filename "data/input09.data"])
  (file->lines filename))

(define (parse-data lines)
  (for/fold ([distances (hash)] [cities (set)]  #:result `(,distances ,(set->list cities)))
            ([line (in-list lines)])
    (match (string-split line)
      [(list a "to" b "=" c)
       (values
        (hash-update distances (set a b) (λ (_) (string->number c)) (λ () (string->number c)))
        (set-union cities (set a b)))])))

(define (segment-length segment distances)
  (let loop ([len 0] [seg segment])
    (cond [(null? seg) len]
          [(eq? (length seg) 1) len]
          [else
           (loop (+ len (hash-ref distances (set (first seg) (second seg)))) (rest seg))])))

(define (path-selector cities distances val cmp)
  (for/fold ([best-path-length val] [best-path '()] #:result best-path-length)
            ([test-path (in-list (permutations cities))])
    (let ([test-path-len (segment-length test-path distances)])
      (cond [(cmp test-path-len best-path-length) (values test-path-len test-path)]
            [else
             (values best-path-length best-path)]))))

(define (main)
  (let* ([data (parse-data (read-data))]
         [distances (first data)]
         [cities (second data)]
         [shortest (path-selector cities distances 99999 <)]
         [longest (path-selector cities distances 0 >)])
    (printf "Part1: ~a\nPart2: ~a\n" shortest longest)))

(main)