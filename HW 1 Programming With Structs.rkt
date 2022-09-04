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
;   was nominated for an Oscar and has a rating of NC-17 or NR
(define (high-brow? F)
  (or
   (and
    (equal? (film-genre F) "Drama")
    (> (film-running-time F) 150))
   (and
    (>= (film-nominations F) 1)
    (or (equal? (film-rating F) "NC-17") (equal? (film-rating F) "NR")))
      ))

(check-expect (high-brow? F1) false)
(check-expect (high-brow? F2) false)
(check-expect (high-brow? F4) true)
(check-expect (high-brow? F5) true)

; (film, film) -> Number
; Takes in two films and returns the sum of the Oscar nominations for both films
(define (total-nominations F1 F2)
  (+ (film-nominations F1) (film-nominations F2)))

(check-expect (total-nominations F2 F4) 4)
(check-expect (total-nominations F3 F1) 4)

; (film, Number) -> film
; Takes in a film and a number and produces a film with the entered number of oscar nominations
(define (update-nominations film num-oscars)
  (make-film (film-title film) (film-genre film) (film-rating film) (film-running-time film) (film-opening-date film) num-oscars))

(check-expect (update-nominations F2 6) (make-film "Sally and the Haunted Balloon" "Horror" "PG-13" 157 D2 6))
(check-expect (update-nominations F3 2) (make-film "Johnnie's Jet" "Science Fiction" "PG-13" 124 D3 2))
(check-expect (update-nominations F1 4) (make-film "Yazzo's Wild Adventures" "Adventure" "PG" 115 D1 4))
(check-expect (update-nominations F4 3) (make-film "Samantha and the Tea Seller" "Drama" "NC-17" 165 D4 3))

; (film, date) -> boolean
; takes in a film and a date and determines if the film was opened before or after the given date

(define (opened-after? Film Date)
  (opened-after-helper (film-opening-date Film) Date))

; HELPER FUNCTION
; (Date Date) -> Boolean
; Takes the date from the film in opened-after and the specified date and determines whether the film was opened before or after the given date
(define (opened-after-helper movie-date Date)
  (cond[(> (date-year movie-date) (date-year Date)) true]
       [(< (date-year movie-date) (date-year Date)) false]
       [else
        (cond[(> (date-month movie-date) (date-month Date)) true]
             [(< (date-month movie-date) (date-month Date)) false]
             [else
              (cond[(> (date-day movie-date) (date-day Date)) true]
                   [(< (date-day movie-date) (date-day Date)) false]
                   [else false])]
                 )]
       ))

(define D3- (make-date 2022 6 16))
(define D4- (make-date 2019 3 21))
(define D6 (make-date 2013 11 3))
(define D7 (make-date 2018 3 22))

(check-expect (opened-after? F2 D1) false)
(check-expect (opened-after? F1 D2) true)
(check-expect (opened-after? F3 D3-) false)
(check-expect (opened-after? F4 D4-) true)
(check-expect (opened-after? F5 D6) true)
(check-expect (opened-after? F1 D7) false)
(check-expect (opened-after? F2 D2) false)


















