#lang racket

(require racket/match)
(require data/queue)

(struct player (hp ap mana [shield #:mutable] [poison #:mutable] [recharge #:mutable]) #:transparent)
(struct boss (hp dp) #:transparent)
(struct state (us them spend) #:transparent)

(define (finished? us them deficit)
  (or (<= (- (player-hp us) deficit) 0) (<= (boss-hp them) 0)))

(define (active-effect? us sh po re)
  (or (and (> sh 0) (> (player-shield us) 0))
      (and (> po 0) (> (player-poison us) 0))
      (and (> re 0) (> (player-recharge us) 0))))

(define-syntax (spell stx)
  (syntax-case stx ()
    [(_ name cost damage armor hitp sh po re)
     #'(define (name old-state [deficit 0])
         (match-let ([(state us them spend) old-state])
           (cond [(finished? us them deficit) '()]
                 [(active-effect? us sh po re) '()]
                 [(< (player-mana us) cost) '()]
                 [else
                  (define np
                    (struct-copy player us [mana (- (player-mana us) cost)]
                                 [hp (+ (- (player-hp us) deficit) hitp)]
                                 [ap (+ (player-ap us) armor)]))
                  (when (> sh 0) (set-player-shield! np sh))
                  (when (> po 0) (set-player-poison! np po))
                  (when (> re 0) (set-player-recharge! np re))
                  (define nb (struct-copy boss them [hp (- (boss-hp them) damage)]))
                  (state np nb (+ spend cost))])))]))
                           
(spell magic-missile 53 4 0 0 0 0 0)
(spell drain 73 2 0 2 0 0 0)
(spell shield 113 0 0 0 6 0 0)
(spell poison 173 0 0 0 0 6 0)
(spell recharge 229 0 0 0 0 0 5)

(define (apply-effects old-state)
  (match-let ([(state us them _) old-state])
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
    (struct-copy state old-state [us np]
                 [them (struct-copy boss them [hp (- (boss-hp them) damage-given)])])))
  
(define (boss-turn old-state)
  (match-let ([(state us them _) old-state])
    (define damage (let ([d (- (boss-dp them) (player-ap us))]) (if (<= d 0) 1 d)))
    (struct-copy state old-state [us (struct-copy player us [hp (- (player-hp us) damage)])])))
                          
(define (player-turn old-state [deficit 0])
  (filter (compose1 not null?)
          (list (magic-missile old-state deficit)
                (drain old-state deficit)
                (shield old-state deficit)
                (poison old-state deficit)
                (recharge old-state deficit))))
  
(define (state-transition old-state [deficit 0])
  (map (compose1 apply-effects boss-turn apply-effects) (player-turn old-state deficit)))

(define (find-cheapest-win start [deficit 0])
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
  (define start-state (state (player 50 0 500 0 0 0) (boss 71 10) 0))
  (printf "Part1: ~a\n" (state-spend (find-cheapest-win start-state)))
  (printf "Part2: ~a\n" (state-spend (find-cheapest-win start-state 1))))

(main)