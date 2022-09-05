;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |HW 1 Programming With Structs|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Date struct: stores a date in year/month/day form
;;year: year of date
;;month: month of date
;;day: day of date
(define-struct date (year month day))
(define D1 (make-date 2018 3 14))
(define D2 (make-date 2016 7 28))
(define D3 (make-date 2022 5 16))
(define D4 (make-date 2019 4 21))
(define D5 (make-date 2013 11 8))

(define-struct film (title genre rating running-time opening-date nominations))
;;interp: represents a film constructor where
;; tile: film's tile
;; genre: film's genre
;; rating: film's rating
;; running-time : the runnig-time of the film
;; openig-date : the date the film opened at the theater
;; nominationsL : the number of oscar nominations the film recieved
(define F1 (make-film "Yazzo's Wild Adventures" "Adventure" "PG" 115 D1 3))
(define F2 (make-film "Sally and the Haunted Balloon" "Horror" "PG-13" 157 D2 4))
(define F3 (make-film "Johnnie's Jet" "Science Fiction" "PG-13" 124 D3 1))
(define F4 (make-film "Samantha and the Tea Seller" "Drama" "NC-17" 165 D4 0))
(define F5 (make-film "Starships Fly" "Science Fiction" "NR" 134 D5 2))
(define film1 (make-film 'passenger 'sci-fi 'PG-13 116 D1 4))
(define film2 (make-film 'dad-im-sorry 'drama 'PG-13 128 D2 1))
(define film3 ( make-film 'squid-game 'survial 'NR 63 D3 35))


;;3
;;film->boolean
;;consume a film to produce a boolean if the film is a drama and has running time more than 150
;or was nomianted and has rating of NC-17 AND NR
(define (high-brow? F)
  (or
   (and
    (equal?(film-genre F)'drama)
    (>(film-runningtime F) 150)
    )
   (and
    (>=(film-nominations F)1)
    (or
     (equal? (film-rating F) 'NC-17)
     (equal? (film-rating F) 'NR)
     )
    )
   )
  )
(check-expect(high-brow? film1)false)
(check-expect(high-brow? film2)false)
(check-expect(high-brow? film3)true)

;;4
;;num num -> num
;;consume number of nominatinos from 2 films and produce the total nominations of 2 films
(define (total-nominations F1 F2)
  (+(film-nominations F1)
    (film-nominations F2)
    )
  )
(check-expect(total-nominations film1 film2)5)
(check-expect(total-nominations film2 film3)36)
(check-expect (total-nominations film1 film3)39)

;;5
; (film, Number) -> film
; Takes in a film and a number and produces a film with the entered number of oscar nominations
(define (update-nominations film num-oscars)
  (make-film (film-title film) (film-genre film) (film-rating film) (film-running-time film) (film-opening-date film) num-oscars))

(check-expect (update-nominations F2 6) (make-film "Sally and the Haunted Balloon" "Horror" "PG-13" 157 D2 6))
(check-expect (update-nominations F3 2) (make-film "Johnnie's Jet" "Science Fiction" "PG-13" 124 D3 2))
(check-expect (update-nominations F1 4) (make-film "Yazzo's Wild Adventures" "Adventure" "PG" 115 D1 4))
(check-expect (update-nominations F4 3) (make-film "Samantha and the Tea Seller" "Drama" "NC-17" 165 D4 3))
(check-expect (update-nominations F5 3) (make-film "Starships Fly" "Science Fiction" "NR" 134 D5 3))

; (film, date) -> boolean
; takes in a film and a date and determines if the film was opened before or after the given date

(define (opened-after? Film Date)
  (greater?
    (film-opening-date Film)
    Date
  )
)

; HELPER FUNCTION
; (Date Date) -> Boolean
; Compares the two dates and return true if the first date is after the second date
(define (greater? D1 D2)
  (cond[(> (date-year D1) (date-year D2)) true]
       [(< (date-year D1) (date-year D2)) false]
       [(> (date-month D1) (date-month D2)) true]
       [(< (date-month D1) (date-month D2)) false]
       [else (> (date-day D1) (date-day D2))]
  )
)

(check-expect (greater? (make-date 2022 6 17) (make-date 2022 6 16)) true)
(check-expect (greater? (make-date 2022 7  1) (make-date 2022 6 16)) true)
(check-expect (greater? (make-date 2023 1  1) (make-date 2022 6 16)) true)
(check-expect (greater? (make-date 2022 6 16) (make-date 2022 6 16)) false)
(check-expect (greater? (make-date 2022 6 15) (make-date 2022 6 16)) false)
(check-expect (greater? (make-date 2022 5 17) (make-date 2022 6 16)) false)
(check-expect (greater? (make-date 2021 7 18) (make-date 2022 6 16)) false)

(define D3- (make-date 2022 6 16))
(define D4- (make-date 2019 3 21))
(define D6  (make-date 2013 11 3))
(define D7  (make-date 2018 3 22))

(check-expect (opened-after? F2 D1) false)
(check-expect (opened-after? F1 D2) true)
(check-expect (opened-after? F3 D3-) false)
(check-expect (opened-after? F4 D4-) true)
(check-expect (opened-after? F5 D6) true)
(check-expect (opened-after? F1 D7) false)
(check-expect (opened-after? F2 D2) false)
