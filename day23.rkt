#lang racket

;; Advent of Code, day 23 -- 2015
;;
;; https://adventofcode.com/2015/day/23
;;
;; Opening the Turing Lock
;;
;; Most of this came from https://docs.racket-lang.org/aoc-racket/Day_23.html
;;
;; Stefan Kruger

(require racket (for-syntax racket/file racket/string))

(define registers (make-hash '((a . 0)(b . 0))))
 
(define-syntax (read-data stx)
  (syntax-case stx ()
    [(_) (let* ([lines (file->lines "data/input23.data")]
                [ls (map (λ(s) (format "(thunk* (instr ~a))" (string-replace s "," " "))) lines)]
                [ld (map (compose1 read open-input-string) ls)])
           (datum->syntax stx `(define instructions (list ,@ld))))]))

(define-syntax-rule (define-reg-fun op f)
  (define (op reg)
    (hash-update! registers reg f)
    1))

(define-reg-fun inc (λ (v) (add1 v)))
(define-reg-fun tpl (λ (v) (* v 3)))
(define-reg-fun hlf (λ (v) (/ v 2)))

(define (jmpf reg offset pred)
  (if (pred (hash-ref registers reg)) offset 1))
    
(define-syntax (instr stx)
  (syntax-case stx (jmp jie jio)
    [(_ jmp offset) #'offset]                                ;; jump absolute
    [(_ jie reg offset) #'(jmpf 'reg 'offset even?)]         ;; jump if even
    [(_ jio reg offset) #'(jmpf 'reg 'offset (curry = 1))]   ;; jump if 1
    [(_ op reg) #'(op 'reg)]))                               ;; inc, tpl or hlf

(read-data)

(define (execute)
  (let loop ([ip 0])
    (cond [(>= ip (length instructions)) (hash-ref registers 'b)]
          [else
           (let ([inst (list-ref instructions ip)])
             (loop (+ (inst) ip)))])))

 (define (main)
   (printf "Part1: ~a\n" (execute))
   (hash-set*! registers 'a 1 'b 0)
   (printf "Part2: ~a\n" (execute)))

(main)
                                          
    
    
