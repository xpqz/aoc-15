#lang racket

;; Advent of Code, day 15 -- 2015
;;
;; https://adventofcode.com/2015/day/15
;;
;; Science for Hungry People
;;
;; https://en.wikipedia.org/wiki/Bijection,_injection_and_surjection
;; https://en.wikipedia.org/wiki/Stars_and_bars_(combinatorics)
;; https://www.statisticshowto.datasciencecentral.com/multiset/
;; https://gist.github.com/rain-1/afe141d455108637654abbe1a85e5843
;;
;; Stefan Kruger

(require racket/stream)

(struct ingredient (name capacity durability flavour texture calories) #:transparent)

(define (ingredients)
  (vector (ingredient 'sugar 3 0 0 -3 2)
          (ingredient 'sprinkles -3 3 0 0 9)
          (ingredient 'candy -1 0 4 0 1)
          (ingredient 'chocolate 0 0 -2 2 8)))

(define (cons/dup y)
  (and (pair? y) (cons (car y) y)))

(define (multiset-inc n state)
  (cond [(null? state) #f]
        [else
         (cond [(= n (car state)) (cons/dup (multiset-inc n (cdr state)))]
               [else
                (cons (add1 (car state)) (cdr state))])]))

(define (multiset k n)
  (let loop ([state (make-list k 1)])
    (cond [state (stream-cons state (loop (multiset-inc n state)))]
          [else '()])))

(define (vector-inc! vec i)
  (vector-set! vec i (add1 (vector-ref vec i))))

(define (bijection k n)
  (λ (state)
    (let ([result (make-vector n 0)])
      (for-each (λ (i) (vector-inc! result (sub1 i))) state)
      result)))

(define (cookie-score proportions)
  (let* ([ingr (ingredients)]
         [data 
          (for/fold ([capacity 0] [durability 0] [flavour 0] [texture 0]
                                  #:result `(,capacity ,durability ,flavour ,texture))
                    ([i (in-range (vector-length proportions))])
            (let ([item (vector-ref ingr i)]
                  [prop (vector-ref proportions i)])
              (values (+ capacity (* prop (ingredient-capacity item)))
                      (+ durability (* prop (ingredient-durability item)))
                      (+ flavour (* prop (ingredient-flavour item)))
                      (+ texture (* prop (ingredient-texture item))))))])
    (apply * (map (λ (x) (if (negative? x) 0 x)) data))))

(define (cookie-find [selector identity])
  (let ([ingr-count (vector-length (ingredients))])
    (for/fold ([best 0] #:result best)
              ([proportions
                (stream-filter selector
                               (stream-map (bijection 100 ingr-count) (multiset 100 ingr-count)))])
      (let ([score (cookie-score proportions)])
        (cond [(> score best) score]
              [else best])))))

(define (500-calories? proportions)
  (let ([cals (for/fold ([cals 0] #:result cals)
                        ([i (in-range (vector-length proportions))])
                (let ([item (vector-ref (ingredients) i)]
                      [prop (vector-ref proportions i)])
                  (+ cals (* prop (ingredient-calories item)))))])
    (= 500 cals)))
 
(define (main)
  (printf "Part1: ~a\n" (cookie-find))
  (printf "Part2: ~a\n" (cookie-find 500-calories?)))

(main)
               