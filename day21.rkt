#lang racket

(require math)  ; sum

(struct player (type hp dp ap) #:transparent)
(struct item (type cost dp ap) #:transparent)

(define weapons
  (list
   (item 'dagger       8 4 0)
   (item 'shortsword  10 5 0)
   (item 'warhammer   25 6 0)
   (item 'longsword   40 7 0)
   (item 'greataxe    74 8 0)))

(define armor
  (list
   (item 'none         0 0 0)
   (item 'leather     13 0 1)
   (item 'chainmail   31 0 2)
   (item 'splintmail  53 0 3)
   (item 'bandedmail  75 0 4)
   (item 'platemail  102 0 5)))

(define rings
  (list
   (item 'none         0 0 0)
   (item 'none         0 0 0)
   (item 'damage+1    25 1 0)
   (item 'damage+2    50 2 0)
   (item 'damage+3   100 3 0)
   (item 'defense+1   20 0 1)
   (item 'defense+2   40 0 2)
   (item 'defense+3   80 0 3)))
      
(define (fight you boss)
  (define (damage a d)
    (max (- (player-dp a) (player-ap d)) 1))
  (let loop ([attacker you] [defender boss])
    (let* ([remaining-hp (- (player-hp defender) (damage attacker defender))]
           [new-defender (struct-copy player defender [hp remaining-hp])])
      (cond [(<= remaining-hp 0) attacker]
            [else (loop new-defender attacker)]))))

;; Fight-rules:
;;
;; 1. Must hold exactly one weapon.
;; 2. May hold zero or one armour items.
;; 3. May hold zero, one or two rings. If two, must be different.
;; 4. P1: Minimise cost whilst winning.
;;    P2: Maximise cost whilst losing.

(define (make-player w a r)
  (define total-cost (sum (map item-cost `(,w ,a ,@r))))
  (define total-dp (sum (map item-dp `(,w ,@r))))
  (define total-ap (sum (map item-ap `(,a ,@r))))
  (values total-cost (player 'me 100 total-dp total-ap)))

(define (find-cost boss winner startval cmpf)
  (for*/fold ([best-cost startval] #:result best-cost)
             ([weapon (in-list weapons)]
              [arm (in-list armor)]
              [ringz (in-combinations rings 2)])
    (define-values (cost you) (make-player weapon arm ringz))
    (cond [(cmpf cost best-cost)
           (match (fight you boss)
             [(player t _ _ _) #:when (equal? t winner) cost]
             [_ best-cost])]
          [else best-cost])))
           

(define (main)
  (printf "Part1: ~a\n" (find-cost (player 'boss 103 9 2) 'me 100000 <))
  (printf "Part2: ~a\n" (find-cost (player 'boss 103 9 2) 'boss 0 >)))
  
(main)