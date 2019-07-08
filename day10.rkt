#lang racket

;; Advent of Code, day 10 -- 2015
;;
;; https://adventofcode.com/2015/day/10
;;
;; Elves Look, Elves Say
;;
;; Note: part 2 requires raising the DrRacket memory limit (512M works).
;;
;; Stefan Kruger


(define (look-and-say start-str count)
  (let loop ([current start-str] [iterations count])
    (cond [(= 0 iterations) (string-length current)]
          [else
           (loop (regexp-replace*
                  #px"(.)\\1*"
                  current
                  (λ (m _) (format "~a~a" (string-length m) (substring m 0 1))))
                 (sub1 iterations))])))
            
(define (main)
  (printf "Part1: ~a\n" (look-and-say "3113322113" 40))
  (printf "Part2: ~a\n" (look-and-say "3113322113" 50)))

(main)