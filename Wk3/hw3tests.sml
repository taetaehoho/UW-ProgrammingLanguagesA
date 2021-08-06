
val test1a = all_except_option("hoing", ["hoing", "boing", "mochibaby"]) = SOME ["boing", "mochibaby"]
val test1b = all_except_option("hng", ["hoing", "boing", "mochibaby"]) = NONE
val test1c = all_except_option("boing", ["hoing", "boing", "mochibaby"]) = SOME ["hoing", "mochibaby"]

val test2a = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2b = get_substitutions1 ([["foo", "hoing", "boing"],["there"]], "foo") = ["hoing", "boing"]
val test2c = get_substitutions1 ([["foo"],["there", "mochi"]], "mochi") = ["there"]
val test2d = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff")= ["Jeffrey","Geoff", "Jeffrey"] 

val test3a = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3b = get_substitutions2 ([["foo", "hoing", "boing"],["there"]], "foo") = ["hoing", "boing"]
val test3c = get_substitutions2 ([["foo"],["there", "mochi"]], "mochi") = ["there"]
val test3d = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff")= ["Jeffrey","Geoff", "Jeffrey"] 

val test4a = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"}) =  
[{first="Fred", last="Smith", middle="W"}, 
{first="Fredrick", last="Smith", middle="W"},
{first="Freddie", last="Smith", middle="W"},
{first="F", last="Smith", middle="W"}]

exception exception1
val test5a = remove_card([(Clubs, Jack), (Spades, Queen), (Clubs, Num 10)], (Clubs, Jack), exception1) = [(Spades, Queen), (Clubs, Num 10)]
val test5b = remove_card([(Clubs, Jack), (Spades, Queen), (Clubs, Num 10)], (Spades, Queen), exception1) = [(Clubs, Jack), (Clubs, Num 10)]

val test6a = all_same_color ([(Clubs, Jack), (Spades, Queen), (Clubs, Num 10)]) = true
val test6b = all_same_color ([(Hearts, Jack), (Diamonds, Queen), (Hearts, Num 10)]) = true
val test6c = all_same_color ([(Clubs, Jack), (Diamonds, Queen), (Hearts, Num 10)]) = false 

val test7a = sum_cards([(Clubs, Jack), (Spades, Queen), (Clubs, Num 10)]) = 30
val test7b = sum_cards([(Clubs, Num 8), (Spades, Queen), (Clubs, Num 10)]) = 28
val test7c = sum_cards([(Clubs, Num 2), (Spades, Num 3), (Clubs, Num 10)]) = 15

val test8a = score([(Clubs, Jack), (Spades, Queen), (Clubs, Num 10)], 5) = 75
val test8b = score([(Clubs, Num 8), (Spades, Queen), (Clubs, Num 10)], 40) = 12
val test8c = score([(Clubs, Num 2), (Spades, Num 3), (Clubs, Num 10)],15) = 0

val test9a = officiate([(Clubs, Jack), (Spades, Queen), (Clubs, Num 10)], [Draw, Draw, (Discard (Clubs, Jack))], 20) = 10