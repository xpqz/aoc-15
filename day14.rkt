#lang racket

;; Advent of Code, day 14 -- 2015
;;
;; https://adventofcode.com/2015/day/14
;;
;; Reindeer Olympics
;;
;; Stefan Kruger


(define (read-data [filename "data/input14.data"])
  (file->lines filename))

(struct reindeer (name speed move-time rest-time)
  #:transparent)

(struct race-entry
  (competitor
   state
   remaining-time
   score
   distance)
  #:transparent)

(define (make-race-entry r)
  (race-entry r 'moving (reindeer-move-time r) 0 0))

(define (parse-data lines)
  (let loop ([reindeers '()] [data lines])
    (cond [(null? data) reindeers]
          [else
           (match (string-split (car data))
             [(list name _ _ speed _ _ mtime _ _ _ _ _ _ rtime _)
              (let* ([numbers (map string->number `(,speed ,mtime, rtime))]
                     [rd (reindeer name (first numbers) (second numbers) (third numbers))])
                (loop (cons rd reindeers) (cdr data)))])])))

(define (state-change re)
  (match re
    [(race-entry (reindeer _ speed move-time rest-time) state _ _ _)
     (match state
       ['moving (struct-copy race-entry re [state 'resting] [remaining-time rest-time])]
       [_       (struct-copy race-entry re [state 'moving] [remaining-time move-time])])]))

(define (sustain-state re)
  (let ([rt-1 (sub1 (race-entry-remaining-time re))])
    (match re
      [(race-entry (reindeer _ speed _ _) state _ _ dist)
       (match state
         ['moving (struct-copy race-entry re [remaining-time rt-1] [distance (+ dist speed)])]
         [_       (struct-copy race-entry re [remaining-time rt-1])])])))

(define (tick re)
  (let ([remaining (race-entry-remaining-time re)])
    (cond [(zero? remaining) (sustain-state (state-change re))]
          [else              (sustain-state re)])))

(define (score lst)
  (let* ([leader-board (sort lst #:key race-entry-distance >)]
         [max (race-entry-distance (car leader-board))])
    (for/list ([re (in-list leader-board)])
      (let ([d (race-entry-distance re)])
        (cond [(eq? d max) (struct-copy race-entry re [score (add1 (race-entry-score re))])]
              [else        re])))))
                                                                 
(define (race racers seconds)
  (let loop ([time 1] [leader-board (score (map tick racers))])
    (cond [(> time seconds) leader-board]
          [else
           (loop (add1 time) (score (map tick leader-board)))])))
  
(define (main)
  (let* ([racers (map make-race-entry (parse-data (read-data)))]
         [final-state (race racers 2503)])
    (printf "Part1: ~a\n" (race-entry-distance (argmax race-entry-distance final-state)))
    (printf "Part2: ~a\n" (race-entry-score (argmax race-entry-score final-state)))))
    
(main)