#lang racket

;; Advent of Code, day 13 -- 2015
;;
;; https://adventofcode.com/2015/day/13
;;
;; Knights of the Dinner Table
;;
;; Stefan Kruger

(define (read-data [filename "data/input13.data"])
  (file->lines filename))

(define (parse-data lines)
  (for/fold ([costs (hash)] [guests (set)]  #:result `(,costs ,(set->list guests)))
            ([line (in-list lines)])
    (match (string-split line)
      [(list person-1 _ gain-or-lose units _ _ _ _ _ _ p-2)
       (let* ([person-2 (string-trim p-2 ".")]
              [person-pair (cons person-1 person-2)]
              [v (string->number units)]
              [signed-units (if (equal? "gain" gain-or-lose) v (- v))])
         (values
          (hash-set costs person-pair signed-units)
          (set-union guests (set person-1 person-2))))])))

(define (couple-cost A B costs)
  (+ (hash-ref costs (cons A B) 0) (hash-ref costs (cons B A) 0)))

(define (happiness-value seating-arrangement start costs)
  (match seating-arrangement
    ['() 0]
    [(list _) (couple-cost start (first seating-arrangement) costs)]
    [_ (+ (couple-cost (first seating-arrangement) (second seating-arrangement) costs)
          (happiness-value (rest seating-arrangement) start costs))]))

(define (seating-selector guests costs)
  (for/fold ([happiest 0] [best-seating '()] #:result happiest)
            ([test-seating (in-list (permutations guests))])
    (let ([test-seating-happiness (happiness-value test-seating (car test-seating) costs)])
      (cond [(> test-seating-happiness happiest) (values test-seating-happiness test-seating)]
            [else
             (values happiest best-seating)]))))

(define (main)
  (let* ([data (parse-data (read-data))]
         [costs (first data)]
         [guests (second data)]
         [happiness (seating-selector guests costs)]
         [happiness2 (seating-selector (cons "Host" guests) costs)])
    (printf "Part1: ~a\n" happiness)
    (printf "Part2: ~a\n" happiness2)))

(main)