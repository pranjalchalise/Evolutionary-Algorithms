(ns mini.playground)

; This project has custom configuration.
; See .vscode/settings.json

; If you are new to Calva, you may want to use the command:
; Calva: Create a “Getting Started” REPL project
; which creates a project with a an interactive Calva (and Clojure) guide.

(println "Hello World")


;; Problem Set 1

;; - Write an expression that calculates the number of seconds in a year.
;; - Define diameter to be some number and write expression that calculates the area
;;   of a circle with that diameter.
;; - Write arithmetic expressions for other formulas that you remember from high school
;;   (or look some up if you don't remember any!).



(* 365 24 60 60)

(def diameter 10)  
(def radius (/ diameter 2))  
(def area (* Math/PI (* radius radius)))  

(println "The area of the circle is:" area)



;; Problem Set 3:
;; - Write functions that calculate, given reasonable numeric inputs, the values 
;;   of the aritmetic/geometric formulae that you provided for Problem Set 1.
;; - Write a function that takes a list of one-syllable words, a list of 
;;   two-syllable words, and a list of three-syllable words, and returns a haiku
;;   constructed from the provided words.

(defn haiku "Generates a haiku" [one-syll two-syll three-syll]
  (concat
   (list
    (cons
     (rand-nth two-syll)
     (list
      (rand-nth three-syll))))
   (list
    (cons
     (rand-nth one-syll)
     (cons (rand-nth one-syll)
           (cons (rand-nth two-syll)
                 (list (rand-nth three-syll))))))
   (list
    (cons
     (rand-nth two-syll)
     (list
      (rand-nth three-syll))))))

(haiku '(mouse god rat dog stream mom house) '(dances jingles hungers babbles punches) '(chocolate gorilla vanilla computer skyscraper))




;; - Factorial using recursion

(defn factorial [n]
  (if(<= n 1)
          1(   * n(factorial(- n 1))
             ) 
  ))
  (println "Factorial of 10 is: " (factorial 10))




;;- Squaring elements in a list

(defn square [x] (* x x))
(println "The squared list of [1 2 3 4 5] is" (map square [1 2 3 4 5]))


;; Filtering even numbers from list and then squaring them

(defn square [x] (* x x))
(defn even[n]
  (zero? (mod n 2)))
(println "The squared list of even numbers from the list [1 2 3 4 5] is" (map square (filter even [1 2 3 4 5])))

  

;; Fizzbuzz  

(defn fizzbuzz []
  (map (fn [n]
         (cond
           (and (zero? (mod n 3)) (zero? (mod n 5))) "FizzBuzz"
           (zero? (mod n 3)) "Fizz"
           (zero? (mod n 5)) "Buzz"
           :else n))
       (range 1 101)))


(doseq [result (fizzbuzz)]
  (println result))










