;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw3-all-problems) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Introduction

;; Spelling Bee is a vocabulary game hosted by the New York Times. The project
;; for this class is to build a program similar to Spelling Bee over the course
;; of several homework assignments. Before reading any further, you should play
;; the New York Times' Spelling Bee to understand the rules of the game:
;;
;; https://www.nytimes.com/puzzles/spelling-bee
;;
;; Your first version of Spelling Bee, which you will design for this
;; assignment, will make several simplifications:
;;
;; 1. You will construct words using five letters, and not seven,
;; 2. You will be able to enter nonsensical words,
;; 3. You will not be able to correct mistakes (i.e., backspace will not
;;      work),
;; 4. You will be able enter the same word several times, and
;; 5. You won't keep score.
;;
;; In later homework, you will remove these restrictions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 1: Representing and Displaying Available Letters
;;

;; Design data called Letters that holds the five letters that are
;; available for your simplified Spelling Bee. Note that one of the letters is
;; distinguished as the required letter (i.e., the letter displayed at the
;; center).
;;
;; Tip: Use "1String" to refer to a one-character string in your data 
;; definition.

;; Letters is a [NEList-of 1String]
;; Represents the letters that are available in the spelling bee game
;; where the first element of the list is the required letter and the remaining
;; elements are the other available letters
;; Examples:
(define LETTERS-1 (list "A" "S" "P" "L" "E"))
(define LETTERS-2 (list "P" "E" "T" "R" "Y"))
(define LETTERS-3 (list "L" "O" "P" "H" "E" "T" "R"))
(define LETTERS-4 (list "T" "G" "O" "I" "R" "E" "N"))
(define LETTERS-5 (list "F" "C" "T" "Y" "U" "L" "A"))
;; Template:
#;(define (letters-temp nelos)
    (cond
      [(empty? (rest nelos)) (... (first nelos) ...)]
      [(cons? (rest nelos)) (... (first nelos) ... (nelos-template (rest nelos)) ...)]))


;; Design a function to display Letters as an Image. Feel free to layout the
;; letters in any way you like. However, the required letter must be
;; distinguished in some way (e.g., by position, color, font, etc).
;;
;; NOTE: You must not use substring in this function, or in any helper function
;; that it employs.

;; display-letters: Letters -> Image
;; Displays the letters in the given Letters as an image.
(check-expect (display-letters LETTERS-1)
              (overlay/align "left" "top"
                             (above (text "A" FONT-SIZE "red")
                                    (foldr beside empty-image
                                           (map (λ (letter) (text letter FONT-SIZE COLOR))
                                                (list "S" "P" "L" "E"))))
                             BACKGROUND))
(check-expect (display-letters LETTERS-3)
              (overlay/align "left" "top"
                             (above (text "L" FONT-SIZE "red")
                                    (foldr beside empty-image
                                           (map (λ (letter) (text letter FONT-SIZE COLOR))
                                                (list "O" "P" "H" "E" "T" "R"))))
                             BACKGROUND))
(check-expect (display-letters LETTERS-5)
              (overlay/align "left" "top"
                             (above (text "F" FONT-SIZE "red")
                                    (foldr beside empty-image
                                           (map (λ (letter) (text letter FONT-SIZE COLOR))
                                                (list "C" "T" "Y" "U" "L" "A"))))
                             BACKGROUND))
              
(define (display-letters letters)
  (overlay/align "left" "top"
                 (above (text (first letters) FONT-SIZE "red")
                        (foldr beside empty-image
                               (map (λ (letter) (text letter FONT-SIZE COLOR))
                                    (rest letters))))
                 BACKGROUND))

;; letter->image : 1String Image -> Image
;; displays an image of the given letter onto the given image
(check-expect (letter->image "A" "red")
              (text "A" FONT-SIZE "red"))
(check-expect (letter->image "B" "black")
              (text "B" FONT-SIZE "black"))
(check-expect (letter->image "C" "orange")
              (text "C" FONT-SIZE "orange"))

(define (letter->image letter color)
  (text letter FONT-SIZE color))


(define WIDTH 400)
(define HEIGHT 300)
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "white"))
(define FONT-SIZE 40)
(define COLOR "black")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 2: Representing the World

;; A game of Spelling Bee must track the available letters and the partial word
;; that the player has entered. Design data called World that holds
;; this data, and design a function called world->image that displays a World as
;; an image. You can produce any image that you like, but it should clearly show
;; both the available letters and the partial word.

;; Note: The final step of this homework has you revise the data definition for
;; World. We recommend completing this step before the revision. However, we
;; recommend you read the whole assignment first so that you can understand
;; the revision that you will have to do.

(define (los-temp los)
  (... 
   (cond [(empty? los) ...]
         [(cons? los) ... (first los) ...
                      ... (los-temp (rest los)) ...])))

;; A World is a (make-world Letters String [NEList-of String] Nat)
(define-struct world [letters partial-word words-so-far points])
;; Represents the available letters, the partial word that has been entered
;; by the player, and the words entered so far
;; Examples:
(define WORLD-1 (make-world LETTERS-1 "APPLE" (list "PALE" "LEAP") 2))
(define WORLD-2 (make-world LETTERS-2 "" '() 0))
(define WORLD-3 (make-world LETTERS-3 "HOLE" (list "POLE" "ROLE") 2))
(define WORLD-4 (make-world LETTERS-4 "" '() 0))
(define WORLD-5 (make-world LETTERS-5 "FACULTY" '() 0))
;; Template:
(define (world-temp world)
  (... (letters-temp (world-letters world)) ...
       (... (world-partial-word world) ...)
       (... (los-temp (world-words-so-far world)) ...)
       (... (world-points world) ...)))

;; world->image: World -> Image
;; Displays a World as an image.
(check-expect (world->image WORLD-1)
              (place-image (display-words-so-far (world-words-so-far WORLD-1))
                           (/ (* WIDTH 3) 4)
                           (/ HEIGHT 2)
                           (place-image (text "APPLE" 30 "black")
                                        (/ WIDTH 4)
                                        (/ HEIGHT 2)
                                        (place-image (text "Score: 2" 20 "black")
                                                     50
                                                     (- HEIGHT 40)
                                                     (display-letters LETTERS-1)))))
(check-expect (world->image WORLD-3)
              (place-image (display-words-so-far (world-words-so-far WORLD-3))
                           (/ (* WIDTH 3) 4)
                           (/ HEIGHT 2)
                           (place-image (text "HOLE" 30 "black")
                                        (/ WIDTH 4)
                                        (/ HEIGHT 2)
                                        (place-image (text "Score: 2" 20 "black")
                                                     50
                                                     (- HEIGHT 40)
                                                     (display-letters LETTERS-3)))))
(check-expect (world->image WORLD-5)
              (place-image (display-words-so-far (world-words-so-far WORLD-5))
                           (/ (* WIDTH 3) 4)
                           (/ HEIGHT 2)
                           (place-image (text "FACULTY" 30 "black")
                                        (/ WIDTH 4)
                                        (/ HEIGHT 2)
                                        (place-image (text "Score: 0" 20 "black")
                                                     50
                                                     (- HEIGHT 40)
                                                     (display-letters LETTERS-5)))))

(define (world->image world)
  (place-image (display-words-so-far (world-words-so-far world))
               (/ (* WIDTH 3) 4)
               (/ HEIGHT 2)
               (place-image (text (world-partial-word world) 30 "black")
                            (/ WIDTH 4)
                            (/ HEIGHT 2)
                            (place-image (text (string-append "Score: "
                                                              (number->string (world-points world)))
                                               20 "black")
                                         50
                                         (- HEIGHT 40)
                                         (display-letters (world-letters world))))))

;; display-words-so-far : [List-of String] -> Image
;; displays the list of string as an image with each word on top of the rest
(check-expect (display-words-so-far (list "hello" "word" "yes"))
              (above (text "Words so far:" 20 "black")
                     (text "hello" 20 "black")
                     (text "word" 20 "black")
                     (text "yes" 20 "black")))
(check-expect (display-words-so-far '()) (text "Words so far:" 20 "black"))
(check-expect (display-words-so-far (list "apple" "banana"))
              (above (text "Words so far:" 20 "black")
                     (text "apple" 20 "black")
                     (text "banana" 20 "black")))

(define (display-words-so-far los)
  (above (text "Words so far:" 20 "black")
         (foldr (λ (string rest) (above (text string 20 "black") rest)) empty-image los)))
                      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 3:  Player Interactions

;; In a game of Spelling Bee, the player can either enter a letter to add to the
;; current word, or press the "Enter" key to complete the word. Design a
;; function with the following signature:

;; key-pressed : World KeyEvent -> World
;; Adds the pressed letter to the current word or completes the word if the
;; "Enter" key is pressed.
(check-expect (key-pressed WORLD-1 "S")
              (make-world LETTERS-1 "APPLES"
                          (list "PALE" "LEAP") 2))
(check-expect (key-pressed WORLD-3 "B")
              (make-world LETTERS-3 "HOLE"
                          (list "POLE" "ROLE") 2))
(check-expect (key-pressed WORLD-3 "\r")
              (make-world LETTERS-3 ""
                          (list "POLE" "ROLE" "HOLE") 3))
(check-expect (key-pressed WORLD-3 "\b")
              (make-world LETTERS-3 "HOL"
                          (list "POLE" "ROLE") 2))

(define (key-pressed world key)
  (cond [(and (string=? "\r" key)
              (passes-checks? world))
         (make-world (world-letters world) ""
                     (append (world-words-so-far world)
                             (list (world-partial-word world)))
                     (award-points world))]
        [(string=? "\b" key)
         (make-world (world-letters world)
                     (remove-last (world-partial-word world))
                     (world-words-so-far world)
                     (world-points world))]
        [(available-key? (world-letters world) (string-upcase key))
         (make-world (world-letters world)
                     (string-append (world-partial-word world) (string-upcase key))
                     (world-words-so-far world)
                     (world-points world))]
        [else world]))

;; available-key?: Letters KeyEvent -> Boolean
;; Checks if the pressed key is an available letter
(check-expect (available-key? LETTERS-1 "J") #f)
(check-expect (available-key? LETTERS-3 "T") #t)
(check-expect (available-key? LETTERS-5 "Z") #f)

(define (available-key? letters key)
  (ormap (λ (letter) (string=? letter key)) letters))

;; remove-last : String -> String
;; Remove the last character of the given string, unless it's already empty.
(check-expect (remove-last "Apple") "Appl")
(check-expect (remove-last "") "")
(check-expect (remove-last "B") "")

(define (remove-last s)
  (if (string=? s "")
      s
      (substring s 0 (- (string-length s) 1))))

;; passes-checks? : World -> Boolean
;; is the entered word:
;; - in the dictionary?
;; - greater than 3 letters?
;; - unique to the list of previously entered words?
;; - and does it contain the required letter?
(check-expect (passes-checks? WORLD-3) #t)
(check-expect (passes-checks? WORLD-1) #t)
(check-expect (passes-checks? WORLD-4) #f)

(define (passes-checks? world)
  (and (in-dictionary? (world-partial-word world) (read-words "words.txt"))
       (more-than-3-letters? (world-partial-word world))
       (unique? (world-partial-word world) (world-words-so-far world))
       (contains-req-letter? world)))

;; in-dictionary? : String -> Boolean
;; checks if the word is in the dictionary
(check-expect (in-dictionary? "APPLE" (read-words "words.txt")) #t)
(check-expect (in-dictionary? "iroiwj" (read-words "words.txt")) #f)
(check-expect (in-dictionary? "TABLE" (read-words "words.txt")) #t)

(define (in-dictionary? word los)
  (ormap (λ (dictionary-word) (string=? (string-downcase word) dictionary-word)) los))

;; more-than-3-letters? : String -> Boolean
;; is the given string greater than 3 letters?
(check-expect (more-than-3-letters? "apple") #t)
(check-expect (more-than-3-letters? "lab") #f)
(check-expect (more-than-3-letters? "") #f)

(define (more-than-3-letters? word)
  (> (string-length word) 3))

;; unique? : String [List-of String] -> Boolean
;; has the given string not been entered before?
(check-expect (unique? "apple" (list "apple" "pale")) #f)
(check-expect (unique? "apple" '()) #t)
(check-expect (unique? "GEMS" (list "MEGA" "GEMS")) #f)

(define (unique? word los)
  (not (ormap (λ (previous-word) (string=? previous-word word)) los)))

;; contains-req-letter? : World -> Boolean
;; does the partial word contain the world's required letter?
(check-expect (contains-req-letter? WORLD-1) #t)
(check-expect (contains-req-letter? WORLD-2) #f)
(check-expect (contains-req-letter? WORLD-5) #t)

(define (contains-req-letter? world)
  (string-contains? (first (world-letters world))
                    (world-partial-word world)))

;; award-points : World -> Nat
;; given a world, produces the corresponding score for the world's partial word
(check-expect (award-points WORLD-1) 4)
(check-expect (award-points (make-world LETTERS-1 "APPLES"
                                        (list "PALE" "LEAP") 2)) 10)
(check-expect (award-points WORLD-5) 11)

(define (award-points world)
  (if
   (andmap (λ (letter) (string-contains? letter (world-partial-word world)))
           (world-letters world))
   (+ (length (world-letters world))
      (- (string-length (world-partial-word world)) 3) (world-points world))
   (+ (- (string-length (world-partial-word world)) 3) (world-points world))))

;; When the key is an available letter, key-pressed should produce a new
;; world that adds that letter to the end of the current word. If the key is not
;; an available letter, it should produce the previous world. If the key is the
;; special string "\r" (which stands for the Enter key), it should produce a new
;; world with the empty string for the current word, as long as the current word
;; contains the center letter. If the player presses any other key, produce the
;; previous world unchanged. In other words, your program should not produce an
;; error if the player presses the "wrong" key.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 4: World Program

;; At this point, you have enough in place to play a basic game. Use the
;; following world program to play spelling bee.

;; play : World -> World
;; Uses big-bang to play a game of Spelling Bee, given Letters.
(define (play w)
  (big-bang
      w
    (to-draw world->image)
    (on-key key-pressed)))

;; [TODO] Click Run. Then, in Interactions, use the function play on an 
;; example of Letters.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 5: Keeping Track of Words

;; The Spelling Bee program that you have so far doesn't even keep track of the
;; words you've entered! We really need the game to show the words that
;; you've already entered. For example, if you previously entered the words
;; "bell" and "well", and the available letters are "b", "o", "w", "e", and "l",
;; with "l" at the center, the program should really display something that
;; looks like this:
;;
;;   O     Words so far:
;; B L W   bell
;;   E     well
;;
;; Here is how you can display text on multiple lines. In a string, the special
;; character "\n" represents a new line. So, the string
;; "Words so far:\nbell\nwell" represents a string that appears on three lines.
;;
;; Modify the data World to include the words so far, represented
;; as a string. You will also have modify the functions you've written so far.
;; But, if you carefully followed the design recipe, the changes will be minor.
;;
;; NOTE: Your program will show the words entered so far, but it will not
;; check that the player does not enter a duplicate word. We will address that
;; problem later.

;; [TODO] Modifications
