#lang racket

;; Advent of Code, day 19 -- 2015
;;
;; https://adventofcode.com/2015/day/19
;;
;; Medicine for Rudolph
;;
;; Stefan Kruger

(define (read-data [filename "data/input19.data"])
  (file->lines filename))

(define (parse-data lines)
  (let loop ([rules '()] [data lines])
    (match-let ([(list line rst ...) data])
      (match (string-split line)
        [(list molecule) (cons rules molecule)]
        ['() (loop rules rst)]
        [(list target "=>" replacement) (loop (cons (cons target replacement) rules) rst)]))))

(define (apply-rule molecule target replacement)
  (let loop ([prefix ""] [results '()] [matches (string-split molecule target #:trim? #f)])
    (match matches
      [(list _) results]
      [_ (match-let* ([(list this rest ...) matches]
                      [tail-str (string-join rest target)]
                      [new-string (string-join (append `(,prefix ,this ,replacement ,tail-str)) "")]
                      [new-prefix (string-join `(,prefix ,this ,target) "")])
           (loop new-prefix (cons new-string results) rest))])))

(define (unique-molecules data)
  (match-let ([`(,rules . ,mol) data])
    (length (remove-duplicates
             (append-map
              (λ (key val) (apply-rule mol key val))
              (map car rules)
              (map cdr rules))))))

(define (step-count rules start-mol)
  ;; Find the smallest possible number of transformations needed.
  ;;
  ;; Apply reverse of rules in order until we get back to "e" or
  ;; rules have no effect. If rules have no effect, inject a bit of
  ;; randomness and try again by shuffling the rules order. This
  ;; works under the assumption there is only one solution from
  ;; e to target.
  (let loop ([current start-mol] [count 0] [rules rules])
    (cond [(equal? current "e") count]
          [else
           (define-values (c txn)
             (for/fold ([c 0] [sample current])
                       ([(from to) (in-parallel (map car rules) (map cdr rules))])
               (values (+ c (length (regexp-match* to sample))) (string-replace sample to from))))

           (cond [(equal? current txn) (loop start-mol 0 (shuffle rules))]
                 [else
                  (loop txn (+ count c) rules)])])))

(define (main)
  (let* ([data (parse-data (read-data))])
    (printf "Part1: ~a\n" (unique-molecules data))
    (printf "Part2: ~a\n" (step-count (car data) (cdr data)))))
    
(main)