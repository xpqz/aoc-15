#lang racket

;; Advent of Code, day 17 -- 2015
;;
;; https://adventofcode.com/2015/day/17
;;
;; No Such Thing as Too Much
;;
;; Stefan Kruger

(require math) ; sum

(define (find-combination-count container-selection target-volume)
  ;; Count number of combinations that make up the target volume
  (for/fold ([count 0] #:result count)
            ([combination (in-combinations container-selection)])
    (if (= (sum combination) target-volume)
        (add1 count)
        count)))

(define (find-combination-count2 container-selection target-volume)
  ;; Find the smallest number of containers required to make target-volume
  ;; and count the number of such combinations.
  (let* ([hist (for/fold ([histogram (hash)] #:result histogram)
                         ([combination (in-combinations container-selection)])
                (if (= (sum combination) target-volume)
                    (let ([key (length combination)])
                      (hash-update histogram key (λ (v) (add1 v)) (λ () 0)))
                    histogram))]
         [smallest (car (sort (hash-keys hist) <))])
    (hash-ref hist smallest)))
         
    

(define (main)
  (let ([data '(50 44 11 49 42 46 18 32 26 40 21 7 18 43 10 47 36 24 22 40)])
    (printf "Part1: ~a\n" (find-combination-count data 150))
    (printf "Part2: ~a\n" (find-combination-count2 data 150))))

(main)