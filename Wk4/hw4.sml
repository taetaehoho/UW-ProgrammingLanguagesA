(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(*1. Write a function only_capitals that takes a string list and returns a string list that has only
the strings in the argument that start with an uppercase letter. Assume all strings have at least 1
character. Use List.filter, Char.isUpper, and String.sub to make a 1-2 line solution*)
val only_capitals = (*string list -> string list*)
    List.filter(fn s => Char.isUpper(String.sub(s, 0)))

(*2. Write a function longest_string1 that takes a string list and returns the longest string in the
list. If the list is empty, return "". In the case of a tie, return the string closest to the beginning of the
list. Use foldl, String.size, and no recursion (other than the implementation of foldl is recursive).*)
val longest_string1 = (*string list -> string*)
    foldl (fn (x,acc) => if String.size x > String.size acc then x else acc) "" 

(*3. Write a function longest_string2 that is exactly like longest_string1 except in the case of ties
it returns the string closest to the end of the list. Your solution should be almost an exact copy of
longest_string1. Still use foldl and String.size.*)
val longest_string2 = 
    foldl (fn(x,acc) => if String.size x >= String.size acc then x else acc) ""

fun longest_string_helper f = 
    foldl (fn (x, acc) => if f(String.size x, String.size acc) then x else acc) ""

val longest_string1 = 
    longest_string_helper (fn (x, acc) => x >= acc)

val longest_string2 = 
    longest_string_helper (fn (x, acc) => x > acc)

(*5. Write a function longest_capitalized that takes a string list and returns the longest string in
the list that begins with an uppercase letter, or "" if there are no such strings. Assume all strings
have at least 1 character. Use a val-binding and the ML library’s o operator for composing functions.
Resolve ties like in problem 2.*)
val longest_capitalized =
    longest_string1 o only_capitals 

(*6. Write a function rev_string that takes a string and returns the string that is the same characters in
reverse order. Use ML’s o operator, the library function rev for reversing lists, and two library functions
in the String module. (Browse the module documentation to find the most useful functions.)*)
val rev_string = 
    implode o rev o explode 

(*7. Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b (notice the 2 arguments are curried). The first argument should be applied to elements of the second argument in order
until the first time it returns SOME v for some v and then v is the result of the call to first_answer.
If the first argument returns NONE for all list elements, then first_answer should raise the exception
NoAnswer. Hints: Sample solution is 5 lines and does nothing fancy.*)
fun first_answer f xs =
    case xs of 
    [] => raise NoAnswer
    | x::xs' => case f x of 
                NONE => first_answer f xs'
                | SOME v => v

(*8. Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option
(notice the 2 arguments are curried). The first argument should be applied to elements of the second
argument. If it returns NONE for any element, then the result for all_answers is NONE. Else the
calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of
all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together (order doesn’t matter).
Hints: The sample solution is 8 lines. It uses a helper function with an accumulator and uses @. Note
all_answers f [] should evaluate to SOME [].*)
fun all_answers f xs =
    let fun all_answers_helper (xs, acc) = 
        case xs of 
        [] => SOME acc
        | x::xs' => case f x of 
                    NONE => NONE
                    | SOME v => all_answers_helper (xs', acc @ v)
    in all_answers_helper (xs, [])
    end

val count_wildcards = g (fn x => 1) (fn y => 0)

val count_wildcards = g (fn x => 1) (fn y => String.size y)

fun count_some_var (s, p) = g (fn x => 0) (fn y => if y = s then 1 else 0) p

fun check_pat p = 
    let
        fun get_list p = 
            case p of 
            Variable x => [x]
            | TupleP ps => List.foldl (fn (x,acc) => get_list(x) @ acc) [] ps    
            | _=> []
        
        fun unique_value x = List.exists (fn y => x = y)

        fun unique_list ps = 
            case ps of 
            [] => true
            | p::ps' => if (unique_value p ps')
                        then false
                        else unique_list ps'
    in unique_list(get_list p)
    end

(*11. Write a function match that takes a valu * pattern and returns a (string * valu) list option,
namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does.
Note that if the value matches but the pattern has no patterns of the form Variable s, then the result
is SOME []. Hints: Sample solution has one case expression with 7 branches. The branch for tuples
uses all_answers and ListPair.zip. Sample solution is 13 lines. Remember to look above for the
rules for what patterns match what values, and what bindings they produce. These are hints: We are
not requiring all_answers and ListPair.zip here, but they make it easier.*)
fun match (v, p) =
    case (v, p) of 
        (_, Wildcard) => SOME []
        | (Const n1, ConstP n2) => if n1 = n2 then SOME [] else NONE
        | (Unit, UnitP) => SOME []
        | (Tuple vl, TupleP pl) => if List.length vl = List.length pl then 
                                    case all_answers match (ListPair.zip(vl, pl)) of 
                                        SOME lst => SOME lst
                                        | _=>NONE
                                    else NONE
        | (Constructor (s1, v1), ConstructorP (s2, p1)) => if s1 = s2 then match(v1, p1) else NONE
        | (_,Variable s) => SOME [(s,v)]
        | (_,_)=>NONE

(*12. Write a function first_match that takes a value and a list of patterns and returns a
(string * valu) list option, namely NONE if no pattern in the list matches or SOME lst where
lst is the list of bindings for the first pattern in the list that matches. Use first_answer and a
handle-expression. Hints: Sample solution is 3 lines.*)

fun first_match v ps = 
    SOME (first_answer (fn p => match(v,p)) ps)
    handle NoAnswer => NONE
