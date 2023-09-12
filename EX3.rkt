#lang pl
#|
<FLANG>::=<num> 1
       | {+ <FLANG> <FLANG> } 2
       | {- <FLANG> <FLANG> } 3
       | {* <FLANG> <FLANG> } 4
       | {/ <FLANG> <FLANG> } 5
       | {with {<id> <FLANG>} <FLANG>} 6
       | <id> 7
       | {fun {<id>} <FLANG>} 8
       | {call <FLANG> <FLANG>} 9
       | {True} 10
       | {False} 11
       | {not <boolean>} 12
       | {< <FLANG> <FLANG>} 13
       | {> <FLANG> <FLANG>} 14
       | {= <FLANG> <FLANG>} 15
       | {if <FLANG> then-do <FLANG> else-do <FLANG>}
       
Where <num> is any number in racket, and <id> is any symbol in racket, and <bool> is true or false
|#


;==== FLANG definition ====
(define-type FLANG
            [Num Number]
            [Add FLANG FLANG]
            [Sub FLANG FLANG]
            [Mul FLANG FLANG]
            [Div FLANG FLANG]
            [With Symbol FLANG FLANG];name, named-expr, body
            [Id Symbol]
            [Fun Symbol FLANG]; parameter-name, body
            [Call FLANG FLANG]
            [Bool Boolean] ;True or False
            [Bigger FLANG FLANG]
            [Smaller FLANG FLANG]
            [Equal FLANG FLANG]
            [Not Boolean];True or False
            [If FLANG FLANG FLANG]
)
#|
In this function we are parsing th sexpr that we are getting in compitbility to the cases of the FLANG languege and we are returning the result of the
Sexpr to the FLANG. When one of the parameters in the FLANG languege is FLANG we are using a recursion.
|#
(: parse-sexpr : Sexpr -> FLANG)

(define (parse-sexpr sxp)
        (match sxp
          [(number: n) (Num n)]
          ['True (Bool true)]
          ['False (Bool false)] 
          [(symbol: name) (Id name)]
          [(cons 'with more)
          ( match sxp
          [(list 'with (list (symbol: name) named-expr) body)
                                 (With name (parse-sexpr named-expr)
                                             (parse-sexpr body))]
             [else (error 'parse-sexpr "parse-sexpr: bad `with' syntax")])]
          [(cons 'fun more)
          ( match sxp
          [(list 'fun (list (symbol: name)) body)
                                 (Fun name (parse-sexpr body))]
             [else (error 'parse-sexpr "bad fun syntax!!")])]
          
          [(list 'call fun-expr arg-expr)
                                           (Call (parse-sexpr fun-expr)
                                                 (parse-sexpr arg-expr))]
          [(list '+ l r) (Add (parse-sexpr l) (parse-sexpr r))]
          [(list '- l r) (Sub (parse-sexpr l) (parse-sexpr r))]
          [(list '* l r) (Mul (parse-sexpr l) (parse-sexpr r))]
          [(list '/ l r) (Div (parse-sexpr l) (parse-sexpr r))]
          [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
          [(list '> lhs rhs) (Bigger (parse-sexpr lhs) (parse-sexpr rhs))]
          [(list '< lhs rhs) (Smaller (parse-sexpr lhs) (parse-sexpr rhs))]
          [(list 'not exp) (match exp
                             ['True (Not true)]
                             ['False (Not false)]
                             [else(error 'parse-sexpr "not true or false ~s" exp)])] 
          [(cons 'if somthing)
           (match sxp
             [(list 'if condition (list 'then-do option1) (list'else-do option2)) (If (parse-sexpr condition) (parse-sexpr option1) (parse-sexpr option2))]
             [else(error 'parse-sexpr "parse-sexpr: bad `if' syntax in ~s" sxp)])]
          [else (error 'parse-sexpr "parse-sexpr: bad syntax in ~s" sxp)]
          
         ))

(test (parse-sexpr (string->sexpr "4")) => (Num 4))

#|
a parsing function, this function converts the string that we are getting to a Sexpr and sending it to the parse-sexpr function above. 
|#
(: parse : String -> FLANG)
(define (parse code)
       (parse-sexpr (string->sexpr code)))

; Tests to the parse function and the parse-sexpr function.

(test (parse "4") => (Num 4))
(test (parse "{+ 3 5}") => (Add (Num 3) (Num 5)))
(test (parse "{+ 3 {- 8 {+ 2 1}}}") => (Add (Num 3) (Sub (Num 8) (Add (Num 2) (Num 1)))))


(test (parse "{with {x {+ 4 2}} {* x x}}") => (With 'x (Add (Num 4) (Num 2))
                                              (Mul (Id 'x) (Id 'x))))

(test (parse "{fun {x} x}") => (Fun 'x (Id 'x)))
(test (parse "{fun {x} {/ x 5}}") => (Fun 'x (Div (Id 'x) (Num 5))))
(test (parse "{call {fun {x} {/ x 5}} 8}") => (Call {Fun 'x (Div (Id 'x) (Num 5))} (Num 8)))
(test (parse "{with {sqr {fun {x} {* x x}}}
                                    {+ {call sqr 5}
                                        {call sqr 6}}}") =>
                 (With 'sqr (Fun 'x (Mul (Id 'x) (Id 'x)))
                       (Add (Call (Id 'sqr) (Num 5))
                            (Call (Id 'sqr) (Num 6)))))
(test (parse "{fun x {* x x}}")=error> "bad fun syntax")
(test (parse "True") => (Bool true))
(test (parse "False") => (Bool false))
(test (parse "{= 3 5}") => (Equal (Num 3) (Num 5)))
(test (parse "{> 3 5}") => (Bigger (Num 3) (Num 5)))
(test (parse "{< 3 5}") => (Smaller (Num 3) (Num 5)))
(test (parse "{not True}") => (Not true))
(test (parse "{not False}") => (Not false))
(test (parse "{not shalom}") =error> "not true or false shalom")
(test (parse "{if {< 3 5} {then-do 4} {else-do 5}}") => (If (Smaller (Num 3) (Num 5)) (Num 4) (Num 5)))  
(test (parse "{+ 1 2 3}") =error> "bad syntax")


#|
 e[v/i]= To substitute all the identifiers that have the same name 'i' in
 the expression 'e' with the expression 'v', replace all the instances
 of 'i', that are free 'e' with the expression 'v'.
======formal specifications of subst ======
1. N[v/x]=N
2. {+ E1 E2}[v/x]={+ E1[v/x] E2[v/x]}
3. {- E1 E2}[v/x]={- E1[v/x] E2[v/x]}
4. {* E1 E2}[v/x]={* E1[v/x] E2[v/x]}
5. {/ E1 E2}[v/x]={/ E1[v/x] E2[v/x]}
6. y[v/x] = y
7. x[v/x] = v
8. {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
9. {with {x E1} E2}[v/x] = {with {y E1[v/x]} E2}
10. {call E1 E2}[v/x] = {call E1[v/x] E2[v/x]}
11. {fun {y} E}[v/x] = {fun {y} E[v/x]}
12. {fun {x} E}[v/x] = {fun {x} E}
13.B[v/x] = B ;; B is Boolean
14.{= E1 E2}[v/x] = {= E1[v/x] E2[v/x]}
15.{> E1 E2}[v/x] = {> E1[v/x] E2[v/x]}
16.{< E1 E2}[v/x] = {< E1[v/x] E2[v/x]}
17.{ not E}[v/x] = {not E[v/x]}
18.{if Econd {then-do Edo} {else-do Eelse}}[v/x]
 = {if Econd[v/x] {then-do Edo[v/x]} {else-do
Eelse[v/x]}} 
|#

(: subst : FLANG Symbol FLANG -> FLANG)
(define (subst expr from to)
        (cases expr
            [(Num n) expr]
            [(Add l r) (Add (subst l from to) (subst r from to))]
            [(Sub l r) (Sub (subst l from to) (subst r from to))]
            [(Mul l r) (Mul (subst l from to) (subst r from to))]
            [(Div l r) (Div (subst l from to) (subst r from to))]
            [(With name named body)
                      (With name (subst named from to)
                            (if (eq? from name)
                                body
                                (subst body from to)))]
          [(Fun name body)
                          (Fun name (if (eq? name from)
                              body
                              (subst  body from to)))]
          [(Call fun-expr arg-expr)  (Call (subst fun-expr from to) (subst arg-expr from to))]
            [(Id name) (if (eq? from name)
                           to
                           expr)]
          [(Bool b) expr]
          [(Equal l r) (Equal (subst l from to) (subst r from to))]
          [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
          [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
          [(Not s) expr]
          [(If c o1 o2) (If(subst c from to) (subst o1 from to) (subst o2 from to))]
         ))

;Tests to sybst function
(test (subst (Mul (Id 'x) (Id 'x)); ==> e
       'x ;==> i
       (Num 6) ;==> v
       ) => (Mul (Num 6) (Num 6)))

(test (subst (Sub (Id 'x) (Id 'x)); ==> e
       'x ;==> i
       (Num 6) ;==> v
       ) => (Sub (Num 6) (Num 6)))


(test (subst (Id 'x)
             'x
             (Num 8)) => (Num 8))

(test (subst (Id 'y)
             'x
             (Num 8)) => (Id 'y))
(test (subst (With 'x (Num 3)
                   (Id 'x))
             'x
             (Num 5)) => (With 'x (Num 3)
                   (Id 'x)))
(test (subst (With 'y
                   (Add (Id 'x) (Num 3))
                   (Add (Id 'x) (Num 5)))
             'x
             (Num 4)) => (With 'y
                               (Add (Num 4) (Num 3))
                               (Add (Num 4) (Num 5))))

(test (subst (Fun 'x (Add (Id 'x) (Id 'y)))
             'x
             (Num 4)) => (Fun 'x (Add (Id 'x) (Id 'y))))

(test (subst (Fun 'x (Add (Id 'x) (Id 'y)))
             'y
             (Num 4)) => (Fun 'x (Add (Id 'x) (Num 4))))
(test (subst (Call (Fun 'x (Div (Id 'x) (Id 'y)))
                   (Add (Id 'x) (Id 'y)))
                   'x
                   (Num 3)) => (Call (Fun 'x (Div (Id 'x) (Id 'y)))
                   (Add (Num 3) (Id 'y))))

(test (subst (Call (Fun 'x (Div (Id 'x) (Id 'y)))
                   (Add (Id 'x) (Id 'y)))
                   'y
                   (Num 3)) => (Call (Fun 'x (Div (Id 'x) (Num 3)))
                   (Add (Id 'x) (Num 3))))
(test (subst (Equal (Id 'x) (Id 'x)); ==> e
       'x ;==> i
       (Num 6) ;==> v
       ) => (Equal (Num 6) (Num 6)))
(test (subst (Bigger (Id 'x) (Id 'x)); ==> e
       'x ;==> i
       (Num 6) ;==> v
       ) => (Bigger (Num 6) (Num 6)))
(test (subst (Smaller (Id 'x) (Id 'x)); ==> e
       'x ;==> i
       (Num 6) ;==> v
       ) => (Smaller (Num 6) (Num 6)))
(test (subst (Bool true)
             'x
             (Bool false)) => (Bool true))
(test (subst (Not true)
             'x
             (Bool false)) => (Not true))
(test (subst (If (Smaller (Id 'x) (Id 'x)) (Num 4) (Id 'x)); ==> e
       'x ;==> i
       (Num 6)) ;==> v
        => (If (Smaller (Num 6) (Num 6)) (Num 4) (Num 6)))


;; The following function is used in multiple places below,
 ;; hence, it is now a top-level definition
(: Num->Number : FLANG -> Number)
;; gets a FLANG -- presumably a Num variant -- and returns the
 ;; unwrapped number 
(define (Num->Number arg)  
        (cases arg
          [(Num n) n]
          [else (error 'Num->number "Num->number: expected a number, got: ~s" arg)]
         ))

;Tests to Num->Number
(test(Num->Number(Num 5)) => 5)
(test(Num->Number(Num -1)) => -1)
(test(Num->Number(Bool #f)) =error> "Num->number: expected a number, got: #(struct:Bool #f")


(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; gets a Racket numeric binary operator, and uses it within a FLANG
 ;; `Num' wrapper
(define (arith-op op arg1 arg2)
     (Num (op (Num->Number arg1) (Num->Number arg2))))

;Tests to arith-op
(test(arith-op + (Num 5) (Num 3)) => (Num 8))
(test(arith-op * (Num 5) (Num -3)) => (Num -15))
(test(arith-op / (Num 6) (Num 3)) => (Num 2))
(test(arith-op - (Num 5) (Num 3)) => (Num 2))


(: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
 ;; gets a Racket Boolean binary operator (on numbers), and applies it
 ;; to two `Num' wrapped FLANGs
 (define (logic-op op expr1 expr2)
   (Bool (op (Num->Number expr1) (Num->Number expr2))))

;Tests to logic-op
(test(logic-op > (Num 5) (Num 3)) => (Bool true))
(test(logic-op < (Num 5) (Num -3)) => (Bool false))
(test(logic-op = (Num 6) (Num 3)) => (Bool false))
(test(logic-op = (Num 12) (Num 12)) => (Bool true))
 
 (: flang->bool : FLANG -> Boolean)
 ;; gets a Flang E (of any kind) and returns a its appropiate
 ;; Boolean value -- which is true if and only if E does not
;; represent false
 ;; Remark: the `flang->bool` function will also be top-level
 ;; since it's used in more than one place.
 (define (flang->bool e)
   (cases e
     [(Bool b) b]
     [else (error 'flang->bool "expected bool, got: ~s" e)]))

;Tests to flang->bool
(test(flang->bool (Bool true)) => true)
(test(flang->bool (Bool false)) => false)
(test(flang->bool (Num 5)) =error>"expected bool, got: #(struct:Num 5)")


#|
formal specifications of eval:

1. eval(N) = N
2. eval({+ E1 E2}) = if(eval(E1), eval(E2)) returns Number, eval(E1) + eval(E2)
                     else: error
3. eval({- E1 E2}) = if(eval(E1), eval(E2)) returns Number, eval(E1) - eval(E2)
                      else: error
4. eval({* E1 E2}) = if(eval(E1), eval(E2)) returns Number, eval(E1) * eval(E2)
                      else: error
5. eval({/ E1 E2}) =  if(eval(E1), eval(E2)) returns Number, eval(E1) / eval(E2)
                      else: error
6. eval({with {x E1} E2}) = eval(E2[(eval E1)/x])
7. eval({fun {x} E})= {fun {x} E}
8. eval({call E1 E2}) = if {fun {x} Ef}<---(eval(E1)):
                            eval(Ef[eval(E2)/x])
                            otherwise: error!!
9.eval(B) = B ;; B is an expression for a Boolean value
10.eval({= E1 E2}) = eval(E1) = eval(E2) \ if both E1 and E2
11.eval({> E1 E2}) = eval(E1) > eval(E2) \ evaluate to numbers
12.eval({< E1 E2}) = eval(E1) < eval(E2) / otherwise error!
13.eval({not E}) = not(eval(E)) /E may be anything
14.eval({if Econd {then-do Edo} {else-do Eelse}})
= eval(Edo) if eval(Econd) =/= false,
eval(Eelse), otherwise.

|#

;;{call {fun {x} {+ x 4}} {call {fun {x} {* 9 x}} 8}}
(: eval : FLANG -> FLANG)
(define (eval expr)
        (cases expr
            [(Num n) expr]
            [(Add l r)  (arith-op + (eval l) (eval r))]
            [(Sub l r) (arith-op - (eval l) (eval r))]
            [(Mul l r) (arith-op * (eval l) (eval r))]
            [(Div l r) (arith-op / (eval l) (eval r))]
            [(With name named body) (eval (subst body name (eval named)))]
            [(Id name) (error 'eval "eval: free identifier: ~s" name)]
            [(Fun name body) expr]
            [(Call fun-expr arg-expr) (let ([fval (eval fun-expr)])
                                        (cases fval
                                          [(Fun name body) (eval (subst body
                                                                        name
                                                                        (eval arg-expr)))]
                                          [else (error 'eval "`call' expected a function, got: ~s" fval)]))]
            [(Bool b) expr]
            [(Equal l r)  (logic-op = (eval l) (eval r))]
            [(Bigger l r)  (logic-op > (eval l) (eval r))]
            [(Smaller l r)  (logic-op < (eval l) (eval r))]
            [(If l m r)
             (let ([res (eval l)])
               (cases res
                 [(Bool g) (match g
                             [#f (eval r)]
                             [#t (eval m)]
                             )]
                 
                 [else (eval m)]))]
            [(Not exp) (Bool(not exp))]
         ))

;Tests to the eval function
(test (eval (Call (Fun 'x (Mul (Id 'x) (Num 4)))
            (Num 3))) => (Num 12))
(test (eval (Call (With 'foo
                  (Fun 'x (Mul (Id 'x) (Num 4)))
                  (Id 'foo))
            (Num 3))) => (Num 12))
(test (eval (Bool true)) => (Bool true))
(test (eval (Bool false)) => (Bool false))
(test (eval (Not true)) => (Bool false))
(test (eval (Not false)) => (Bool true))
(test (eval (Equal (Num 5) (Num 12))) => (Bool false))
(test (eval (Equal (Num 5) (Num 5))) => (Bool true))
(test (eval (Bigger (Num 5) (Num 5))) => (Bool false))
(test (eval (Smaller (Num 5) (Num 5))) => (Bool false))
(test (eval (Bigger (Num 6) (Num 5))) => (Bool true))
(test (eval (Smaller (Num 4) (Num 5))) => (Bool true))
(test (eval (If (Smaller (Num 3) (Num 2)) (Num 4) (Num 5))) => (Num 5))
(test (eval (If (Equal (Num 3) (Num 3)) (Num 4) (Num 5))) => (Num 4))


#|
we will allow the interface procedure to return any one of the three
possible types of the extended language.
|#
(: run : String ->  (U Number Boolean FLANG))
;; evaluate a FLANG program contained in a string 
(define (run code)
  (let ([res (eval (parse code))])
        (cases res
          [(Num n) n]
          [(Bool b) b]
          [else res])))


;Test for the run function
(test (run "5") => 5)
(test (run "{+ 4 6}") => 10)
(test (run "{+ 4 6}") => 10)
(test (run "{call {fun {x} {+ x 7}} {with {x 8} {+ x 7}}}") => 22)
(test (run "True") => true)
(test (run "{not True}") => false)
(test (run "{> 3 44}") => false)
(test (run "{= 3 44}") => false)
(test (run "{= 44 44}") => true)
(test (run "{> 46 44}") => true)
(test (run "{< 44 44}") => false)
(test (run "{> 44 44}") => false)
(test (run "{< 43 44}") => true)
(test (run "true") =error> "eval: free identifier: true")
(test (run "{< false 5}") =error> "eval: free identifier: false")
(test (run "{< False 5}")
 =error> "Num->number: expected a number, got: #(struct:Bool #f)")
(test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4) 
(test (run "{with {x 8}
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
(test (run "{with {x 0}
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
(test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
(test (run "{with {c True}
 {if c {then-do {> 2 1}} {else-do 2}}}")
 => true)
(test (run "{with {foo {fun {x}
 {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}")
 => (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2))))) 
(test (run "{with {x 0}
 {if {> x 0} {/ 2 x} x}}")
 =error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")
 (test (run "true") =error> "eval: free identifier: true")
(test (run "{< false 5}") =error> "eval: free identifier: false")
(test (run "{< False 5}")
 =error> "Num->number: expected a number, got: #(struct:Bool #f)")
(test (run "{call False 4}") =error> "eval: `call' expected a function, got")
(test (run "{with x 2 {if {> x 2} {/ 2 x} x}}") =error> "parse-sexpr: bad `with' syntax")








