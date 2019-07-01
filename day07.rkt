#lang racket

;; Advent of Code, day 7 -- 2015
;;
;; https://adventofcode.com/2015/day/7
;;
;; Some Assembly Required
;;
;; Stefan Kruger

(require racket/string)

(struct gate (inputs operation output) #:transparent)

(define (read-data [filename "data/input07.data"])
  (for/list ([line (in-list (file->lines filename))])
    (map (λ (x) (let ([n (string->number x)]) (if n n x))) (string-split line))))

(define (parse-line lst)
  (match lst
    [(list a "AND" b "->" c) (gate `(,a ,b) bitwise-and c)]
    [(list a "OR" b "->" c) (gate `(,a ,b) bitwise-ior c)]
    [(list a "RSHIFT" b "->" c) (gate `(,a ,b) (λ (v n) (arithmetic-shift v (- n))) c)]
    [(list a "LSHIFT" b "->" c) (gate `(,a ,b) arithmetic-shift c)]
    [(list "NOT" a "->" b) (gate `(,a) bitwise-not b)]
    [(list a "->" b) (gate `(,a) identity b)]))

(define (evaluate wires term)
  (cond [(number? term) term]
        [else (hash-ref wires term)]))

(define (value g wires)
  (let ([all-inputs-filled (for/and ([inp (in-list (gate-inputs g))])
                             (or
                              (number? inp)
                              (hash-ref wires inp #f)))])
    (cond [all-inputs-filled
           (apply (gate-operation g) (map (curry evaluate wires) (gate-inputs g)))]
          [else #f])))

(define (rotate lst)
  (flatten (append (cdr lst) (car lst))))

(define (resolve spec initial-wires)
  (let loop ([gates spec] [wires initial-wires])
    (cond [(null? gates) wires]
          [else
            (let* ([current-gate (car gates)]
                   [target (gate-output current-gate)]
                   [gate-val (value current-gate wires)])
              (cond [gate-val
                     ;; Update the wires hash. Note that if a wire already have a value, it
                     ;; should NOT be overwritten, as hinted at in the problem statement:
                     ;;
                     ;; "Each wire can only get a signal from one source, but can provide its
                     ;;  signal to multiple destinations."
                     (let ([new-wires (hash-update wires target (λ (v) v) (λ () gate-val))])
                       (loop (cdr gates) new-wires))]
                    [else
                     (loop (rotate gates) wires)]))])))

(define (part1 data)
  (let* ([gates (map parse-line data)]
         [wires (resolve gates (hash))])
    (hash-ref wires "a")))

(define (part2 data v)
  (let* ([gates (map parse-line data)]
         [wires (resolve gates (hash "b" v))])
    (hash-ref wires "a")))

(define (main)
  (let* ([data (read-data)]
         [p1 (part1 data)]
         [p2 (part2 data p1)])
    (printf "Part1: ~a\nPart2: ~a\n" p1 p2)))

(main)
