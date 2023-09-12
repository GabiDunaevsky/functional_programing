#lang pl
#|
In this question we were requierd to build a function that is getting Listof(Listof Number) and creating one list of the numbers.
I chose to do this function recursivly (as most of the functions on racket), the way i did it is by runing untill the last list inside
the Listof(Listof Number) and after that recursivly adding by using the append function the inner lists untul i am am getting one list of numbers,
my stop condition is when i dont have inner lists anymore.
difficulties and overcome: it was the first function, therefor understanding the syntax of the language was a difficult but by watching some examples
was manage to overcome this.
time to solve the question: 10 minutes.
|#
(: open-list : (Listof(Listof Number)) -> (Listof Number))
(define (open-list arr)
  (cond[(null? arr) null]
       [else(append(first arr) (open-list(rest arr)))]))
#| Test question 1.1: |#
(test(open-list '((1 2 3)(4 5 6))) => '(1 2 3 4 5 6))
(test(open-list '(()()()())) => '())
(test(open-list '()) => '()) 
(test(open-list '((1 2 3)()(8 9 10))) => '(1 2 3 8 9 10))
(test(open-list '((1 2 3)(4 5 6 7 8 9 10))) => '(1 2 3 4 5 6 7 8 9 10))
(test(open-list '((1)( 2 3 4 5 6 7 8 9 10)())) => '(1 2 3 4 5 6 7 8 9 10))
(test(open-list '((1)(2) (3) (4) (5)())) => '(1 2 3 4 5))
(test(open-list '((1))) => '(1))

#|
In this question we were requierd to build a function that is getting Listof(Listof Number) and creating a list where the first element is
the min number and the second element is the max number , since we were requierd to compare the values with +inf and - inf but in the case that
the list is empty of elements we were requierd to return a list in the form of [-inf +inf] i made a helper function that firstly checking if the list
is empty and if the list is empty i am returning the list as requierd if the list is not empty I chose to do this function recursivly
(as most of the functions on racket), the way i did it is by runing untill the last list inside
the Listof(Listof Number) and then puting a stop condition that returning a list with [+inf.0 -inf.0] for compering the numbers.
i am running untill i dont have numbers anymore and then by recurtion i am compering the numbers by using min and max build function and puting
the result inside the list.Every time i am sending a new listof lists by using cons function without the first element.
Another condition is when i am finishing with inner list and i am passing directly to the next inner list because i have nothing to compere.
difficulties and overcome:Again the syntax was a difficulty but also the need of using another functions inside the recursion,by watching some examples
was manage to overcome this.
time to solve the question: 15 minutes.
|#

(: min&max : (Listof(Listof Number)) -> (Listof Number))
  (define (min&max arr)
    (cond[(null? (open-list arr)) (list -inf.0 +inf.0)]
    [else (min&max2 arr)]))

(: min&max2 : (Listof(Listof Number)) -> (Listof Number))
  (define (min&max2 arr)
    (cond[(null? arr) (list +inf.0 -inf.0)]
         [(null? (first arr)) (min&max2(rest arr))]
    [else(list(min(first(first arr)) (first(min&max2(cons(rest(first arr))(rest arr))))) (max(first(first arr)) (second(min&max2(cons(rest(first arr))(rest arr))))))]))

#| Test question 1.2: |#
(test(min&max '(( 1 2 3)(-1 -2 -3))) => '(-3.0 3.0))
(test(min&max '(()())) => '(-inf.0 +inf.0))
(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0))
(test(min&max '()) => '(-inf.0 +inf.0))
(test (min&max '((1) (2 3 3 4) (9 2 -1) (233 90))) => '(-1.0 233.0))
(test (min&max '((1))) => '(1.0 1.0))
(test(min&max '(( 1/2 0 0.25)(-1 -2 -3.5))) => '(-3.5 0.5))

#|
In this functio we were requierd to do the same as the min&max but by using apply function. The way i solved it is by using the first function to make
 a list and after that by using apply function to find the min and the max and put it inside a list, if i am getting a list of nulls i am returning -inf and +inf.
difficulties and overcome:find how apply works, i found the decomendation.
time to solve the question: 5 minutes.
|#

(: min&max_apply : (Listof(Listof Number)) -> (Listof Number))
   (define (min&max_apply arr)
     (cond[(null? (open-list arr)) (list -inf.0 +inf.0)]
     [else(list(apply min(open-list arr)) (apply max (open-list arr)))]))

#| Test question 1.3: |#
(test(min&max_apply '(( 1 2 3)(-1 -2 -3))) => '(-3 3))
(test(min&max_apply '(()())) => '(-inf.0 +inf.0))
(test (min&max_apply '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1 233))
(test(min&max_apply '()) => '(-inf.0 +inf.0))
(test (min&max_apply '((1) (2 3 3 4) (9 2 -1) (233 90))) => '(-1 233))
(test (min&max_apply '((1))) => '(1 1))
(test(min&max_apply '(( 1/2 0 0.25)(-1 -2 -3.5))) => '(-3.5 0.5))
#|
Here i just created a type with two constarctors as requiers in the task.
difficulties and overcome: didnt have.
time to solve the question: 2 minutes.
|#

(define-type Table
  [EmptyTbl]
  [Add Symbol String Table])

#| Test question 2.1 and 2.2: |#
(test (EmptyTbl) => (EmptyTbl))
(test (Add 'b "B" (Add 'a "A" (EmptyTbl))) => (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) => (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))
(test (Add 'c "hello"(Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl))))) => (Add 'c "hello"(Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl))))))

#|
In this function we were requierd to find a simbol in the table and returnning the string that represented by the simbol, if the simbol dosnt exist
we need to return false.
I used the cases function, when the table is empty i am returning false(if i am getting to an emptytable) and if not i am cheking two options with
cond function 'if the simbols are equale i am returning the string, if not by recursion i am going to the next table ( the one that inside the first one)
difficulties and overcome: to send the cases function the parameters in a correct way, after a lot of attempts and printings i understood.
time to solve the question: 30 minutes.
|#

(: search-table : Symbol Table -> (U String Boolean))
   (define (search-table sym tbl)
       (cases tbl
         [(EmptyTbl) #f]
         [(Add n w f) (cond [(eq? sym n) w][else(search-table sym f)])]))

#| Test question 2.1 and 2.3: |#
(test (search-table 'a (Add 'a "Correct answer" (Add 'b "wrong" (Add 'a "wrong" 
(EmptyTbl)))))=> "Correct answer")
(test (search-table 'c (Add 'a "wrong" (Add 'b "wrong" (Add 'a "wrong" 
(EmptyTbl)))))=> #f)
(test (search-table 'a (Add 'c "wrong" (Add 'b "wrong" (Add 'a "Correct answer" 
(EmptyTbl)))))=> "Correct answer")
(test (search-table 'a (EmptyTbl)) => #f)
(test (search-table 'c (Add 'c "Correct answer" (Add 'c "wrong" (Add 'b "wrong" (Add 'a "wrong" 
(EmptyTbl))))))=> "Correct answer")

#|
In this function we were requierd to delete a simbol and a string in the table and returnning a new table without them, if the simbol dosnt exist
we need to return the whole list.
I used the cases function, when the table is empty i am returning the original table and if not i am cheking two options with
cond function 'if the simbols are equale i am returning the i am returning the next table, if not i am adding by recursion the last table
 and i am going to check the next table ( the one that inside the last table) therefor i am getting the element before the deleted function
and the elements after them.
difficulties and overcome: to understand how to add the elements before the elemet i need to delete,i just thought a little bit about the recursion.
time to solve the question:10 minutes.
|#

 (: remove-item : Table Symbol -> Table)
   (define (remove-item tbl sym)
       (cases tbl
         [(EmptyTbl) (EmptyTbl)]
         [(Add n w f) (cond [(eq? sym n) f][else(Add n w (remove-item f sym))])]))

#| Test question 2.1 and 2.4: |#
(test (remove-item (Add 'a "LaLa" (Add 'b "NoNo" (Add 'a "YaYa" 
(EmptyTbl)))) 'a) => (Add 'b "NoNo" (Add 'a "YaYa" (EmptyTbl))))

(test (remove-item (Add 'a "LaLa" (Add 'b "NoNo" (Add 'a "YaYa" 
(EmptyTbl)))) 'b) => (Add 'a "LaLa" (Add 'a "YaYa" (EmptyTbl))))

(test (remove-item (Add 'a "LaLa" (Add 'b "NoNo" (Add 'a "YaYa" 
(EmptyTbl)))) 'c) => (Add 'a "LaLa" (Add 'b "NoNo" (Add 'a "YaYa" (EmptyTbl)))))

(test (remove-item  (EmptyTbl) 'a) => (EmptyTbl))

       
