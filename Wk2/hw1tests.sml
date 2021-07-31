val test1a = is_older ((1,2,3),(2,3,4)) = true
val test1b = is_older ((2,3,3),(2,3,4)) = true
val test1c = is_older ((4,2,3),(2,3,4)) = false
val test1d = is_older ((1,2,3),(1,2,3)) = false

val test2a = number_in_month ([(1,2,3), (3,4,5), (3,2,5)], 2, 0) = 2
val test2b = number_in_month ([(1,4,3), (3,4,5), (3,4,5)], 4, 0) = 3
val test2c = number_in_month ([(1,2,3), (3,4,5), (3,2,5)], 1, 0) = 0
val test2d = number_in_month ([], 2, 0) = 0

val test3a = number_in_months ([(1,2,3), (3,4,5), (3,2,5)], [2, 4], 0) = 3
val test3b = number_in_months ([(1,4,3), (3,4,5), (3,4,5)], [4], 0) = 3
val test3c = number_in_months ([(1,2,3), (3,4,5), (3,2,5)], [1,2], 0) = 2
val test3d = number_in_months ([], [1], 0) = 0
val test3e = number_in_months ([(1,2,3)], [], 0) = 0

val test4a = dates_in_month ([(1,2,3), (3,4,5)], 2, []) = [(1,2,3)]
val test4b = dates_in_month ([(1,2,3), (3,4,5)], 4, []) = [(3,4,5)]
val test4c = dates_in_month ([(1,2,3), (3,4,5)], 6, []) = []

val test5a = dates_in_months ([(1,2,3), (3,4,5)], [2], []) = [(1,2,3)]
val test5b = dates_in_months ([(1,2,3), (3,4,5)], [2,4], []) = [(3,4,5), (1,2,3)]
val test5c = dates_in_months ([(1,2,3), (3,4,5)], [], []) = []

val test6a = get_nth (["hoing", "boing", "moing"], 2, 1) = "boing"
val test6b = get_nth (["hoing", "boing", "moing"], 3, 1) = "moing"
val test6c = get_nth ([], 2, 1) = ""

val test7a = date_to_string(1,2,3) = "February 3, 1"
val test7b = date_to_string(1998,5,7) = "May 7, 1998"
val test7c = date_to_string(2000, 10, 10) = "October 10, 2000"

val test8a = number_before_reaching_sum(12, [1,2,3,4,5,6,7,8,9], 0) = 4
val test8b = number_before_reaching_sum(16, [1,2,3,4,5,6,7,8,9], 0) = 5
val test8c = number_before_reaching_sum(3, [1,2,3,4,5,6,7,8,9], 0) = 1

val test9a = what_month(70) = 3
val test9b = what_month(364) = 12
val test9c = what_month(2) = 1
val test9d = what_month(31) = 1

val test10a = month_range(33, 35, []) = [2,2,2]
val test10b = month_range(1, 3, []) = [1,1,1]

val test11a = oldest([(1999, 5, 7), (1997, 5, 7), (1999, 4, 7), (1997, 5, 6)]) = SOME (1997, 5 ,6)
val test11b = oldest([(1999, 4, 7), (1999, 4, 6)]) = SOME (1999, 4 ,6)
val test11c = oldest([]) = NONE