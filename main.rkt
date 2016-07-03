#lang racket

(require srfi/13)
(require megaparsack)

(define (main)
  (begin
    (define my-game (make-game))
    (start-game my-game)))

(struct game (location-edges location-attributes current-location has-food where-is-food))
(struct location-attribute (info))

(define (make-game)
  (let*
      ((locations (start-locations))
       (where-is-food (choose-randomly locations))
       (all-locations (cons start-location locations))
       (location-edges (generate-location-edges all-locations))
       (location-attributes (generate-location-attributes all-locations)))
    (game location-edges location-attributes start-location #f where-is-food)))

(define (start-locations) (generate-random-locations base-edge-count))
(define (choose-randomly locations) (random-element locations))

(define (random-element lst)
  (list-ref lst (random (length lst))))

(define (generate-location-edges locations)
  (let*
      ((random-pair (lambda () (cons (random-element locations) (random-element locations))))
       (pairs (replicate-call (* 2 base-edge-count) random-pair))
       (empty-edges (eval (cons hash (skip locations ''())))))
    (foldr (lambda (indices edges) (link (car indices)
                                         (cdr indices)
                                         edges))
           empty-edges pairs)))

(define (skip lst element)
  (if (null? lst)
      '()
      (cons (car lst) (cons element (skip (cdr lst) element)))))

(define (link index-a index-b edges)
  (let*
      ((edges0 (hash-set edges  index-a (cons index-b (hash-ref edges index-a))))
       (edges1 (hash-set edges0 index-b (cons index-a (hash-ref edges index-b)))))
    edges1))

(define (replicate-call count f)
  (if (> count 0)
      (cons (f) (replicate-call (- count 1) f))
      '()))

(define (generate-location-attributes locations)
  (eval (cons hash (skip locations '(location-attribute "it exists...")))))

(define (intersperse element lst)
  (if (and (not (null? lst)) (not (null? (cdr lst))))
      (cons (car lst)
            (cons element (intersperse element (cdr lst))))
      lst))

(define (show-location location) location)

(define (show-locations locations)
  (string-concatenate (intersperse " " (map show-location locations))))

(define (nearby-locations game) #f)

(define (loop-game game)
  (let*
      ([line (read-line)]
       [cmd '('quit)])
    (cond
      ([(eq? (car cmd) 'act)
        (let*
            ([game-dids-pair (step-game game (cdr cmd))]
             [dones (map play-did (cdr game-dids-pair))])
          (if (eval (cons 'and dones))
              (void)
              (loop-game (car game-dids-pair))))]
       [(eq? (car cmd) 'quit)
        (display "bye!")]))
    (void)))

(define (step-game game act)
  (let
      ([nearbys (nearby-locations game)]
       [loc (cdr act)])
    (if (member loc nearbys)
        (move game loc)
        (cons game '([cons 'log "you can't go that way"])))))

(define (play-did did)
  (let
      ([tag (car did)]
       [datum (cdr did)])
    (cond
      ([(eq? tag 'log) (begin (display datum) #f)]
       [(eq? tag 'died) (begin (display "you died.") (display datum) #t)]
       [(eq? tag 'win) (begin (display "you win!.") (display datum) #t)]))))

(define (move game location) #f)

(define (start-game game)
  (begin
    (display "you're in the basement. find food and come back!\n")
    (display (string-concatenate (list "nearby locations: " (show-locations (nearby-locations game)))))
    (loop-game game)))

(define (generate-random-locations count)
  (let*
      ([locations (generate-locations)]
       [locations-length (length locations)])
    (if (> count locations-length)
        (append locations (generate-random-locations (- count locations-length)))
        (take locations count))))

(define (generate-locations)
  (let* ([location-length (min (length location-adjectives) (length location-nouns))]
         [take-some (lambda (lst) (take (shuffle lst) location-length))])
    (map (lambda (adjective noun) (string-concatenate (list adjective "-" noun)))
       (take-some location-adjectives)
       (take-some location-nouns))))

(define base-edge-count 40)

(define start-location "basement")

(define location-adjectives
  '("red"
    "blue"
    "green"
    "orange"
    "dark"
    "silly"
    "light"
    "purple"
    "scary"
    "happy"
    "gloomy"
    "sketchy"
    "carpeted"
    "slippery"
    "drafty"
    "lofty"
    "ruined"
    "spacious"
    "dodgy"
    "sealthy"
    "doggy"
    "tiny"
    "baby"
    "little"
    "massive"
    "old-fashioned"
    "scared"
    "quiet"
    "lukewarm"
    "popular"
    "bitter"
    "thinking"
    "acid"
    "fishy"
    "extra"
    "main"
    "master"
    "mild"
    "sharp"
    "fancy"
    "subtle"
    "tangy"
    "wild"
    "local"
    "unusual"
    "moist"
    "wet"
    "arid"
    "dainty"
    "sour"
    "salty"
    "pungent"
    "natural"
    "other"
    "good"
    "great"
    "spicy"
    "wonderful"
    "mellow"
    "metallic"
    "new"
    "old"
    "magic"
    "bland"
    "amazing"
    "average"
    "normal"
    "typical"
    "above-average"
    "decent"
    "five-star"
    "squeaky"
    "slient"
    "noisy"
    "grand"
    "lazy"
    "shabby"
    "raggedy"
    "dull"
    "mushy"
    "worn-down"
    "mythic"
    "secret"
    "standard"
    "giant"
    "starry"
    "cold"
    "warm"
    "hot"
    "humid"
    "frozen"
    "sweet"
    "lovely"
    "glamorous"
    "busy"
    "idle"
    "unspoiled"
    "pristine"
    "interesting"
    "rusty"
    "low-quality"
    "mid-quality"
    "starchy"
    "pickled"
    "screeching"
    "misty"
    "abandoned"
    "robust"
    "last"
    "first"
    "glitter"
    "bloody"
    "ancient"
    "outdoor"
    "charming"
    "private"
    "public"
    "temporary"
    "dank"
    "janky"
    "hacky"))

(define location-nouns
  '("room"
    "foxhole"
    "dungeon"
    "lobby"
    "clubhouse"
    "place"
    "avenue"
    "arena"
    "habor"
    "quarters"
    "chamber"
    "shed"
    "house"
    "shelter"
    "crib"
    "cardboard-box"
    "hole-in-the-wall"
    "hut"
    "lodge"
    "cottage"
    "campground"
    "street"
    "area"
    "corner"
    "territory"
    "post-office"
    "office"
    "forest"
    "town"
    "villa"
    "land"
    "wasteland"
    "tavern"
    "workshop"
    "shoppe"
    "yard"
    "garden"
    "tent"
    "path"
    "road"
    "trail"
    "hill"
    "destination"
    "location"
    "spot"
    "hotel"
    "patch-of-land"
    "country-club"
    "swamp"
    "condo"
    "shack"
    "terrain"
    "spot"
    "lot"
    "property"
    "cabin"
    "pool-house"
    "bar"
    "rest-stop"
    "park"
    "gazebo"
    "tarp"
    "reservation"
    "alley"
    "valley"
    "cave"
    "mine"
    "mineshaft"
    "ranch"
    "bay"
    "sewer"
    "fair"
    "mall"
    "market"))