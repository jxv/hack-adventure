#lang racket

(require srfi/13)

(struct game (location-edges location-attributes current-location has-food where-is-food))

(define (make-game)
  (let*
      ((locations (start-locations))
       (where-is-food (choose-randomly locations))
       (all-locations (cons start-location locations))
       (location-edges (generate-edges all-locations))
       (location-attributes (generate-location-attributes)))
    (game location-edges location-attributes start-location #f where-is-food)))


(define (start-locations) (generate-random-locations base-edge-count))
(define (choose-randomly locations) #f)
(define (generate-edges locations) #f)
(define (generate-location-attributes) #f)

(define (intersperse element lst)
  (if (and (not (null? lst)) (not (null? (cdr lst))))
      (cons (car lst)
            (cons element (intersperse element (cdr lst))))
      lst))

(define (show-location location) #f)

(define (show-locations locations)
  (string-concatenate (intersperse " " (map show-location locations))))

(define (nearby-locations game) #f)
(define (loop game) #f)

(define (start game)
  (begin
    (display "you're in the basement. find food and come back!\n")
    (display (string-concatenate (list "nearby locations: " (show-locations (nearby-locations game)))))
    (loop game)))

(define (generate-random-locations count)
  (let*
      ((locations (generate-locations))
       (locations-length (length locations)))
    (if (> count locations-length)
        (append locations (generate-random-locations (- count locations-length)))
        (take locations count))))

(define (generate-locations)
  (let* ((location-length (min (length location-adjectives) (length location-nouns)))
         (take-some (lambda (lst) (take (shuffle lst) location-length))))
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