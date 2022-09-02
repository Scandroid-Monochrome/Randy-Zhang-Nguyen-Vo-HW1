;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |HW 1 Programming With Structs|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Date struct: stores a date in year/month/day form
(define-struct date (year month day))
(define D1 (make-date 2018 3 14))
(define D2 (make-date 2016 7 28))
(define D3 (make-date 2022 5 16))
(define D4 (make-date 2019 4 21))
(define D5 (make-date 2013 11 8))

; Film struct: stores the neccessary info about a film, including the name, genre, rating, running time(in minutes), opening date, and how many Oscars it has
(define-struct film (title genre rating running-time opening-date nominations))
(define F1 (make-film "Yazzo's Wild Adventures" "Adventure" "PG" 115 D1 3))
(define F2 (make-film "Sally and the Haunted Balloon" "Horror" "PG-13" 157 D2 4))
(define F3 (make-film "Johnnie's Jet" "Science Fiction" "PG-13" 124 D3 1))
(define F4 (make-film "Samantha and the Tea Seller" "Drama" "NC-17" 165 D4 0))
(define F5 (make-film "Starships Fly" "Science Fiction" "NR" 134 D5 2))


; Film -> boolean
; Takes in a film and returns true if the film is a drama more than 150 minutes
;   OR
;   was nominated for an Oscar and has a rating of NC-13 or NR
(define (high-brow? F)
  (or
   (and
    (= (film-genre F) "Drama")
    (> (film-running-time F) 150))
   (and
    (>= (film-nominations F) 1)
    (or (= (film-rating F) "NC-17") (= (film-rating F) "NR")))
      ))

(check-expect (high-brow? F1) false)
(check-expect (high-brow? F2) false)
(check-expect (high-brow? F4) true)
(check-expect (high-brow? F5) true)

; (film, film) -> Number
; Takes in twwo films and returns the sum of the Oscar nominations for both films
(define (total-nominations F1 F2)
  (+ (film-nominations F1) (film-nominations F2)))

(check-expect (total-nominations F2 F4) 4)
(check-expect (total-nominations F3 F1) 4)












