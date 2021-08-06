(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
   
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(*1. Write a function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
to compare strings.*)
fun all_except_option (s1, xs) =
    let fun contains (s1, xs) = 
        case xs of  
        [] => []
        | head::rest => if same_string(head, s1) then contains(s1, rest) else head::contains(s1, rest)
        val x = contains(s1, xs)
    in if length x = length xs then NONE else SOME x
    end

(*2. Write a function get_substitutions1, which takes a string list list (a list of list of strings, the
substitutions) and a string s and returns a string list. The result has all the strings that are in
some list in substitutions that also has s, but s itself should not be in the result.*)
fun get_substitutions1 (xs, s1)= (*string list list * string -> string list*)
    case xs of 
    [] => []
    | x::xs' => case all_except_option(s1, x) of
                NONE => get_substitutions1(xs', s1)
                | SOME y => y @ get_substitutions1 (xs', s1)

(*3.  Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
local helper function.*)
fun get_substitutions2 (xs, s1) = (*string list list * string -> string list*)
    let fun get_subs(xs, s1, acc) = 
        case xs of 
        [] => acc
        | x::xs' => case all_except_option(s1, x) of
            NONE => get_subs(xs', s1, acc)
            | SOME y => get_subs (xs', s1, acc @ y)
    in get_subs(xs, s1, [])
    end

(*4. Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and
(c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
names (type {first:string,middle:string,last:string} list). The result is all the full names you
can produce by substituting for the first name (and only the first name) using substitutions and parts (b)
or (c). The answer should begin with the original name (then have 0 or more other names).*)
fun similar_names (xs, {first=first, middle=middle, last=last}) = (*string list list * fullname -> fullname list*)    
    case xs of
    [] => []
    | head::rest => let val first_names = get_substitutions2(xs, first) (*string list list * string -> string list*)
                    (*create list of possible substitutions for first*)
                        fun create_name_record (xs : string list) = (*string list -> fullname list*)
                    (*takes list of strings and creates record list with each element x middle last*)
                            case xs of 
                            [] => []
                            | x::xs' => {first= x, middle = middle, last = last}::create_name_record(xs')
                    in {first=first, middle=middle, last=last}::create_name_record(first_names)
                    end
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*5. Write a function card_color, which takes a card and returns its color (spades and clubs are black,
diamonds and hearts are red)*)
fun card_color (s,r) = 
    case s of 
    Clubs => Black | Spades => Black
    | Diamonds => Red | Hearts => Red

(*6. Write a function card_value, which takes a card and returns its value (numbered cards have their
number as the value, aces are 11, everything else is 10).*)
fun card_value (s,r) = 
    case r of 
    Num x => x | Ace=>11 |_=>10

(*7. Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
If c is not in the list, raise the exception e*)
fun remove_card(cs, c, e) = 
    case cs of 
    [] => raise e
    | head::cs' => if head = c then cs' 
                   else head::remove_card(cs', c, e)

(*8. Write a function all_same_color, which takes a list of cards and returns true if all the cards in the
list are the same color*)
fun all_same_color(cs) = (*card list -> boolean*)
 case cs of 
 [] => true
 | head::[] => true
 | head::(neck::tail) => card_color(head) = card_color(neck) andalso all_same_color(neck::tail)

(*9. Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally
defined helper function that is tail recursive.*)
fun sum_cards(cs) = (*card list -> integer*)
    let fun sum_cards(cs, acc) = 
        case cs of 
        [] => acc
        | c::cs' => sum_cards(cs',acc+card_value(c))
    in sum_cards(cs, 0)
    end

(*10. Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes
the score as described above.*)
fun score(heldcards, goal) =
    let val sum = sum_cards(heldcards)
    in if sum > goal then 3*(sum-goal) else goal-sum
    end

(*Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list
(what the player “does” at each point), and an int (the goal) and returns the score at the end of the
game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive
helper function that takes several arguments that together represent the current state of the game.*)
fun officiate(cs, movelist, goal) = (*cardlist, move list (list of moves), int -> int*)
(*takes list of cards, list of moves and a goal int and produces the final score when either score > goal or movelist empty*)
    let fun current_state(cs, movelist, goal, hand, acc) =
            case movelist of 
            [] => acc
            | move::movelist' => case move of 
                                 Discard c => let val acc_discard = score(remove_card(hand, c, IllegalMove), goal)
                                              in current_state(cs, movelist', goal, remove_card(hand, c, IllegalMove), acc_discard) end
                                 | Draw => case cs of 
                                            [] => acc
                                            | card::cs' => if sum_cards(card::hand) > goal then score(card::hand, goal)
                                                           else current_state(cs', movelist', goal, card::hand, score(card::hand, goal))
    in current_state(cs, movelist, goal, [], 0)
    end 

        