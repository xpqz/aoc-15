#lang racket

;; Advent of Code, day 6 -- 2015
;;
;; https://adventofcode.com/2015/day/6
;;
;; Probably a Fire Hazard
;;
;; Stefan Kruger

(require math)

(define (read-data [filename "data/input06.data"])
  (file->lines filename))

(define (parse-data data)
  (for/list ([line (in-list data)])
    (let ([matched (cdr (regexp-match #px"(turn on|turn off|toggle)\\s+(\\d+),(\\d+)\\s+through\\s+(\\d+),(\\d+)" line))])
      `(,(car matched) ,(map string->number (cdr matched))))))

(define (coord-list coords)
  (match-let ([(list x1 y1 x2 y2) coords])
    (for*/list ([x (in-range x1 (add1 x2))]
                [y (in-range y1 (add1 y2))])
      (cons x y))))

(define (make-updater-part1 op)
  (match op
    ["turn on" (λ (_) 1)]
    ["turn off" (λ (_) 0)]
    [_ (λ (v) (if (zero? v) 1 0))]))

(define (make-updater-part2 op)
  (match op
    ["turn on" add1]
    ["turn off" (λ (v) (if (> v 0) (sub1 v) v))]
    [_ (λ (v) (+ v 2))]))

(define (set-canvas canvas instruction updater)
  (match-let ([(list op coords) instruction])
    (let loop ([cv canvas] [todo (coord-list coords)])
      (cond [(empty? todo) cv]
            [else
             (loop (hash-update cv (car todo) (updater op) 0) (cdr todo))]))))
  
(define (execute data updater)
  (let loop ([instr-list data] [canvas (hash)])
    (cond [(empty? instr-list) canvas]
          [else
           (loop (cdr instr-list) (set-canvas canvas (car instr-list) updater))])))

(define (main)
  (for ([updater (in-list `(,make-updater-part1 ,make-updater-part2))])
    (let ([canvas (execute (parse-data (read-data)) updater)])
      (printf "~a\n" (sum (hash-values canvas))))))
  
       
(main)