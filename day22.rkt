#lang racket

(require racket/match)
(require data/queue)

(provide (all-defined-out))

(struct player (hp ap mana shield poison recharge) #:transparent)
(struct boss (hp dp) #:transparent)
(struct state (us them spell spend) #:transparent)

(define MM-COST 53)
(define DR-COST 73)
(define SH-COST 113)
(define PO-COST 173)
(define RE-COST 229)

(define (finished? us them deficit)
  (or (<= (- (player-hp us) deficit) 0) (<= (boss-hp them) 0)))
  
(define (magic-missile old-state deficit)
  ;; If we can afford it, cast magic missile
  (match-let ([(state us them _ spend) old-state])
    (cond [(finished? us them deficit) '()]
          [(>= (player-mana us) MM-COST)
           (define np (struct-copy player us [mana (- (player-mana us) MM-COST)] [hp (- (player-hp us) deficit)]))
           (define nb (struct-copy boss them [hp (- (boss-hp them) 4)]))
           (state np nb 'magicmissile (+ spend MM-COST))]
          [else '()])))

(define (drain old-state deficit)
  ;; If we can afford it, cast drain
  (match-let ([(state us them _ spend) old-state])
    (cond [(finished? us them deficit) '()]
          [(>= (player-mana us) DR-COST)
           (define np (struct-copy player us [mana (- (player-mana us) DR-COST)] [hp (- (+ (player-hp us) 2) deficit)]))
           (define nb (struct-copy boss them [hp (- (boss-hp them) 2)]))
           (state np nb 'drain (+ spend DR-COST))]
          [else '()])))

(define (shield old-state deficit)
  ;; If we can afford it, and we don't already have one running, cast shield
  (match-let ([(state us them _ spend) old-state])
    (cond [(finished? us them deficit) '()]
          [(and (<= (player-shield us) 0) (>= (player-mana us) SH-COST))
           (define np (struct-copy player us [mana (- (player-mana us) SH-COST)] [shield 6] [hp (- (player-hp us) deficit)]))
           (state np them 'shield (+ spend SH-COST))]
          [else '()])))

(define (poison old-state deficit)
  ;; If we can afford it, and we don't already have one running, cast poison
  (match-let ([(state us them _ spend) old-state])
    (cond [(finished? us them deficit) '()]
          [(and (<= (player-poison us) 0) (>= (player-mana us) PO-COST))
           (define np (struct-copy player us [mana (- (player-mana us) PO-COST)] [poison 6] [hp (- (player-hp us) deficit)]))
           (state np them 'poison (+ spend PO-COST))]
          [else '()])))

(define (recharge old-state deficit)
  ;; If we can afford it, and we don't already have one running, cast recharge
  (match-let ([(state us them _ spend) old-state])
    (cond [(finished? us them deficit) '()]
          [(and (<= (player-recharge us) 0) (>= (player-mana us) RE-COST))
           (define np (struct-copy player us [mana (- (player-mana us) RE-COST)] [recharge 5] [hp (- (player-hp us) deficit)]))
           (state np them 'recharge (+ spend RE-COST))]
          [else '()])))

(define (apply-effects old-state)
  (match-let ([(state us them _ _) old-state])
    (define (dec0 v) (if (> v 0) (sub1 v) 0))
    (define damage-given (if (> (player-poison us) 0) 3 0))
    (define armour (if (> (player-shield us) 0) 7 0))
    (define new-mana (if (> (player-recharge us) 0) 101 0))
    (define np (struct-copy player us
                            [mana (+ (player-mana us) new-mana)]
                            [ap armour]
                            [shield (dec0 (player-shield us))]
                            [poison (dec0 (player-poison us))]
                            [recharge (dec0 (player-recharge us))]))
    (struct-copy state
                 old-state
                 [us np]
                 [them (struct-copy boss them [hp (- (boss-hp them) damage-given)])])))
  
(define (boss-turn old-state)
  (match-let ([(state us them _ _) old-state])
    (define damage (let ([d (- (boss-dp them) (player-ap us))]) (if (<= d 0) 1 d)))
    (struct-copy state old-state [us (struct-copy player us [hp (- (player-hp us) damage)])])))
                          
(define (player-turn old-state [deficit 0])
  (filter (compose1 not null?)
          (list (magic-missile old-state deficit)
                (drain old-state deficit)
                (shield old-state deficit)
                (poison old-state deficit)
                (recharge old-state deficit))))
  
(define (state-transition old-state deficit)
  (map (compose1 apply-effects boss-turn apply-effects) (player-turn old-state deficit)))

(define (find-cheapest-win start [deficit 0])
  ;; Breadth-first search for lowest-cost win. 
  (define (winner? s)
    (<= (boss-hp (state-them s)) 0))
  (define Q (make-queue))
  (enqueue! Q start)
  (define visited (mutable-set start))
  (define winner (struct-copy state start [spend +inf.0]))
  (let loop ()
    (cond [(queue-empty? Q) winner]
          [else
           (define current (dequeue! Q))
           (for-each
            (λ (neighbour)
              (unless (set-member? visited neighbour)
                (enqueue! Q neighbour)
                (set-add! visited neighbour)
                (when (and (winner? neighbour) (< (state-spend neighbour) (state-spend winner)))
                  (set! winner neighbour))))
                (state-transition current deficit))
              (loop)])))

                
(define (main)
  (define start-state (state (player 50 0 500 0 0 0) (boss 71 10) 'start 0))
  (printf "Part1: ~a\n" (state-spend (find-cheapest-win start-state 0)))
  (printf "Part2: ~a\n" (state-spend (find-cheapest-win start-state 1))))

(main)
  
