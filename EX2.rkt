#lang pl 02
#|
// On every step we wrote a number so it will be clear to understed what is the step.
We also treating<D> as a finite sequence of numbers as wrote in the question, there for when we deriving a function to <D> it is like a string of digits.
We wrote a few words on every stage of the grammer.

1.a- // the main derivation with the function as requierd in the task. 
1.<SE>::=1.1 <#\SE-Dig> (Char)
       1.2 <Number>
       1.3 <"<D>"> (String)
       1.4 | { string <SE-Char> }
       1.5 | {string-append <SE-Sequence-Strings> } 
       1.6 | {string-length <"Number"> }
       1.7| {string-insert <SE-Strings> <#\SE-Dig> <SE-Numbers>}
       1.8 | {number->string <SE-Numbers>}

//Only strings represents only the function that derivating strings.
2.<SE-Strings>::= 2.1 <"<D>"> (String)
       2.2 | { string <SE-Char> }
       2.3 | {string-append <SE-Sequence-Strings> }
       2.4 | {string-insert <SE-Strings> <#\SE-Dig> <SE-Numbers> }
       2.5| {number->string <SE-Numbers>}


//only Numbers represents the numbers that derivatin only numbers
3.<SE-Numbers>::=3.1 <Number>
       3.2 | {string-length <"<D>">}

// to ger the digits as chars. 
4.<SE-Dig>::=4.1 <0>
           4.2 <1>
           4.3 <2>
           4.4 <3>
           4.5 <4>
           4.6 <5>
           4.7 <6>
           4.8 <7>
           4.9 <8>
           4.10 <9>

   //to ge as many digits as we want (for example to append function)
5.<SE-Char>::=5.1 <#\SE-Dig>
             5.2 | <#\SE-Dig> <SE-Char>

//to get as many strings as we want including the empty string

6.<SE-Sequence-Strings>::=6.1 <"<D>">
                         6.2 <λ>   //empty string
              6.3 | <"<D>"> <SE-Sequence-Strings>
              6.4 | <λ> <SE-Sequence-Strings>
              6.5 | <SE-Strings>
              6.6 | <SE-Strings> <SE-Sequence-Strings>

// the empty string as requierd.

7.<λ>::="" // empty string


// We wrote 2 derivations that represents the expressions : number->string,string-length,string-append,string.

1.b-    (1.8)                             (3.2)                                           (3.2)                  (1.8)
- <SE> => ( number->string <SE-Numbers> ) => ( number->string ( string-length "76575" ) ) => ( number->string 5) => "5"
       (1.5)                                     (6.6)                                                  (2.2)                                                       (5.2)
- <SE> => ( string-append <SE-Sequence-Strings>) => ( string-append <SE-Strings> <SE-Sequence-Strings>) => (string-append (string <SE-Char>) <SE-Sequence-Strings>) => (string-append (string <#\SE-Dig> <SE-Char>) <SE-Sequence-Strings>)
       (4.2)                                                             (5.1)                                                              (4.3)                                                        
       => (string-append (string <#\1> <SE-Char>) <SE-Sequence-Strings>) => (string-append (string <#\1> <#\SE-Dig>) <SE-Sequence-Strings>) => (string-append (string <#\1> <#\2) <SE-Sequence-Strings>)
       (2.2)                                         (6.1)                       (1.5)
       => (string-append "12" <SE-Sequence-Strings>) => (string-append "12" "12") => "1212"

|#

;2.

#|
In this function we supposed to reture the sum of the squerd elements inside the list.
because we don't habe the lambda function in pl 02 we created an helper function that calculating the square for a number.
by using map we run on all the elements and calculating their square.
at the ens we sum all the elements using foldl and getting the reasult.
|#

(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
  (cond
    [(null? lst)(error 'sum-of-squares "no elements in the list")] 
  [else(foldl + 0 (map sqr lst))]))

(: sqr : Number -> Number)
(define (sqr num)
  (* num num))


;;tests for sum-of-squares:

(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(-1 -2 -3)) => 14)
(test (sum-of-squares '(1)) => 1)
(test (sum-of-squares '(1 -2)) => 5)
(test (sum-of-squares '()) =error> "sum-of-squares: no elements in the list")
(test (sum-of-squares '(1 -2 3 -6 11 -17)) => 460)
(test (sum-of-squares '(0 0 0 0 0 0)) => 0)

;3.a
#|
in this question we were requierd to create the creatPolynomial function using a templet that was given us in the task by to inner functions.
Generally in the createPolynomial we are creating a procedure (PolyX), this is an inner function that is taking a number (the x)
and calling another inner function (poly) that calculating recursively the reasult of the polynom.
|#

(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
  (: poly : (Listof Number) Number Integer Number ->
     Number)
  (define (poly argsL x power accum)
    (cond[(null? argsL)accum]
    [else(poly (rest argsL) x (+ power 1) (+ accum (* (first argsL) (expt x power))))]))
  (: polyX : Number -> Number)
  (define (polyX x)
    (poly coeffs x 0 0))
  polyX)

 ;;tests for question 3.a
(define p2345 (createPolynomial '(2 3 4 5)))
(define p536 (createPolynomial '(5 3 6)))
(define p_0 (createPolynomial '()))
(define p-536 (createPolynomial '(-5 -3 6)))


(test (p2345 0) => (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3)))) 
(test (p2345 4) => (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3)))) 
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3)))) 
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2))))
(test (p_0 4) => 0)
(test (p536 -2) => (+ (* 5 (expt -2 0)) (* 3 (expt -2 1)) (* 6 (expt -2 2))))
(test (p-536 19) => (+ (* -5 (expt 19 0)) (* -3 (expt 19 1)) (* 6 (expt 19 2))))




#|3.b.1
// here we were requiered to build a bnf to a new languege Plang as requierd in the task.
// because the two lists of the plang has to be bigger then 1 so we created a <AE-loop> that "forcing" the lists of the PLANG to be
// bigger then 1.

<PLANG>::=
       | {{poly <AE-loop>} {<AE-loop>))

<AE>::=<Number>
       | {+ <AE> <AE> }
       | {- <AE> <AE> }
       | {* <AE> <AE> }
       | {/ <AE> <AE> }

<AE-loop>::=
           |<AE>
           |<AE> <AE-loop>
|#

;; 3.b.2
#|
In this function we also go a structure to work on, we were requierd to build a parser to the PLANG languege.
On our pare we checked using match all the options that can be to the sexp we used the tests to understend the requierments also.
we also put a condition that if the sexp dosent match to any of the errors in the test so we just sending a bad syntax error.
|#
(define-type PLANG 
    [Poly (Listof AE) (Listof AE)]) 
 
  (define-type AE 
    [Num  Number] 
    [Add  AE AE] 
    [Sub  AE AE] 
    [Mul  AE AE] 
    [Div  AE AE]) 
 
  (: parse-sexpr : Sexpr -> AE) 
  ;; to convert s-expressions into AEs 
  (define (parse-sexpr sexpr) 
    (match sexpr 
      [(number: n)    (Num n)] 
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))] 
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))] 
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))] 
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))] 
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) 
   
  (: parse : String -> PLANG)
  (define (parse str)
  ;; parses a string containing a PLANG expression         to a PLANG AST   (define (parse str) 
    (let ([code (string->sexpr str)]) 
      (match code
        [(list(list 'poly) (list x ...))(error 'parse "at least one coefficient is required in ~s" code)]
        [(list(list 'poly x ...) '()) (error 'parse "at least one point is required in ~s" code)]
        [(list(list 'poly x ...)(list y ...))(Poly(map parse-sexpr x)(map parse-sexpr y))]
        [else (error 'parse "bad syntax in ~s" code)])))


(test (parse "{{poly 1 2 3} {1 2 3}}")  => (Poly (list (Num 1) (Num 2) (Num 3)) (list (Num 1) (Num 2) (Num 3)))) 
(test (parse "{{poly } {1 2} }") =error> "parse: at least one coefficient is required in ((poly) (1 2))") 
(test (parse "{{poly 1 2} {} }") =error> "parse: at least one point is required in ((poly 1 2) ())")
(test (parse "{{2} {1}}") =error> "parse: bad syntax in ((2) (1))")
(test (parse "{{poly } {1} }") =error> "parse: at least one coefficient is required in ((poly) (1))")



;3.b.c
#|
In this function we also go a structure to work on, we were requierd to build an evaluator to the PLANG languege.
we run the run function that evaluates the numbers inside the PLANG structure, We are sending the string to the parser to get the
PLANG expression after that we are calling the eval-poly function tha for every element inside the PlANG lists (Which are AE elements)
using map evaluets using the evel function we got as a structure and returning it at the end.
|#

;; evaluates AE expressions to numbers

  (: eval : AE -> Number )
  (define (eval expr) 
    (cases expr 
      [(Num n)  n] 
      [(Add l r) (+ (eval l) (eval r))] 
      [(Sub l r) (- (eval l) (eval r))] 
      [(Mul l r) (* (eval l) (eval r))] 
      [(Div l r) (/ (eval l) (eval r))]))

  (: eval-poly : PLANG ->  (Listof Number) ) 
  (define (eval-poly p-expr) 
    (cases p-expr
      [(Poly r l) (map(createPolynomial(map eval r)) (map eval l))]))
 
  (: run : String -> (Listof Number)) 
  ;; evaluate a FLANG program contained in a string 
  (define (run str) 
    (eval-poly (parse str))) 

;;tests for 3.b.3
(test (run "{{poly 1 2 3} {1 2 3}}")  => '(6 17 34)) 
(test (run "{{poly 4 2 7} {1 4 9}}")  => '(13 124 589)) 
(test (run "{{poly 1 2 3} {1 2 3}}")   => '(6 17 34)) 
(test (run "{{poly 4/5 } {1/2 2/3 3}}")  => '(4/5 4/5 4/5)) 
(test (run "{{poly 2 3} {4}}")  => '(14)) 
(test (run "{{poly 1 1 0} {-1 3 3}}")  => '(0 4 4))  
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") => '(14)) 
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}") => '(0 4 4)) 









