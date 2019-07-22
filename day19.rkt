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
    
(define (apply-rule molecule key replacement)
  (let loop ([prefix ""] [results '()] [matches (string-split molecule key #:trim? #f)])
    (match matches
      [(list _) results]
      [_ (match-let* ([(list this rest ...) matches]
                      [tail-str (string-join rest key)]
                      [new-string (string-join (append `(,prefix ,this ,replacement ,tail-str)) "")]
                      [new-prefix (string-join `(,prefix ,this ,key) "")])
           (loop new-prefix (cons new-string results) rest))])))

(define (unique-molecules data)
  (set-count (list->set (append-map (λ (r) (apply-rule (cdr data) (car r) (cdr r))) (car data)))))

;; Part 2: starting at "e" what's the lowest number of transformations required to get
;; to the target molecule? We can solve this by going backwards: at each state, only
;; rules whose replacement is present in the molecule could have been applied.
(define (step-count rules molecule)
  (let loop ([target molecule] [count 0])
    (cond [(equal? target "e") count]
          [else
           (printf "target: ~a -> " target)
           (let ([count&target
                  (for/fold ([c 0] [s target] #:result (cons c s))
                            ([r (in-list rules)] #:when (string-contains? target (cdr r)))
                    (values (add1 c) (string-replace s (cdr r) (car r) #:all? #f)))])
             (printf "~a\n" (cdr count&target))
             (cond [(equal? target (cdr count&target))
                    (step-count (shuffle rules) molecule)] ;; dead end. Shuffle rule-order and re-try
                   [else
                    (loop  (cdr count&target) (+ count (car count&target)))]))])))
                 
(define (main)
  (let ([data (parse-data (read-data "data/test.data"))])
    (printf "Part1: ~a\n" (unique-molecules data))
    (printf "Part2: ~a\n" (step-count (car data) (cdr data)))))
    
;(main)